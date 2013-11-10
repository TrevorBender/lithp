{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ExistentialQuantification #-}

import                  Control.Applicative ((<$>), (*>), (<*), (<*>))
import                  Control.Monad (liftM)
import      "mtl"       Control.Monad.Error
import                  Data.IORef
import                  System.Environment (getArgs)
import                  System.IO
                            ( hGetContents
                            , stdin
                            , hFlush
                            , stdout
                            , Handle(..)
                            , IOMode(..)
                            , openFile
                            , hClose
                            , hGetLine
                            , hPrint
                            )
import                  Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params  :: [String]
                    , vararg  :: (Maybe String)
                    , body    :: [LispVal]
                    , closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- Parsing ------------------------------------------------------------------{{{
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Monadic style parsers ------------------------------------------------------{{{

parseStringM :: Parser LispVal
parseStringM = do
    char '"'
    x <- many $ noneOf "\""
    char '"'
    return $ String x

parseAtomM :: Parser LispVal
parseAtomM = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
                  "#t" -> Bool True
                  "#f" -> Bool False
                  _    -> Atom atom

parseNumberM :: Parser LispVal
parseNumberM = liftM (Number . read) $ many1 digit

parseExprM :: Parser LispVal
parseExprM = parseAtomM
         <|> parseStringM
         <|> parseNumberM
         <|> parseQuotedM
         <|> do char '('
                x <- try parseListM <|> parseDottedListM
                char ')'
                return x

parseListM :: Parser LispVal
parseListM = liftM List $ sepBy parseExprM spaces

parseDottedListM :: Parser LispVal
parseDottedListM = do
    head <- endBy parseExprM spaces
    tail <- char '.' >> spaces >> parseExprM
    return $ DottedList head tail

parseQuotedM :: Parser LispVal
parseQuotedM = do
    char '\''
    x <- parseExprM
    return $ List [Atom "quote", x]
--}}}

-- Applicative style parsers --------------------------------------------------{{{

parseStringA :: Parser LispVal
parseStringA = String <$> (char '"' *> many (noneOf "\"") <* char '"')

parseAtomA :: Parser LispVal
parseAtomA = res <$> ((:) <$> first <*> rest)
    where first = letter <|> symbol
          rest = many $ letter <|> digit <|> symbol
          res atom = case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumberA :: Parser LispVal
parseNumberA = (Number . read) <$> many1 digit

a <||> b = try a <|> b

parseComment :: Parser ()
parseComment = char ';' *> many (noneOf "\n") *> char '\n' *> return ()

parseExprA :: Parser LispVal
parseExprA = parseAtomA
         <|> parseStringA
         <|> parseNumberA
         <|> parseQuotedA
         <|> char '(' *> (parseListA <||> parseDottedListA) <* char ')'

parseListA :: Parser LispVal
parseListA = List <$> (sepBy parseExprA spaces)

parseDottedListA :: Parser LispVal
parseDottedListA = DottedList <$> head <*> tail
    where head = endBy parseExprA spaces
          tail = char '.' *> spaces *> parseExprA

parseQuotedA :: Parser LispVal
parseQuotedA = (List . toList) <$> (char '\'' *> parseExprA)
    where toList x = [Atom "quote", x]

readExprA :: String -> LispVal
readExprA input = case parse parseExprA "lisp" input of
                      Left err  -> error $ "No match: " ++ show err
                      Right val -> val
--}}}

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                                Left err  -> throwError . Parser $ err
                                Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExprA

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ do
    skipMany parseComment
    sepEndBy1 parseExprA (spaces >> skipMany parseComment)

--}}}

-- Error ----------------------------------------------------------------------{{{

showError :: LispError -> String
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (NotFunction message func) = message ++ ": " ++ func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch typ found) = "Type mismatch, Expected " ++ typ ++ " ; found " ++ show found
showError (BadSpecialForm str val) = "Bad form " ++ str ++ " : " ++ show val
showError (UnboundVar str val) = "Unbound " ++ str ++ " : " ++ val
showError (Default str) = str

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError ::  IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

extractIOValue :: IOThrowsError a -> IO a
extractIOValue val = extractValue <$> (runErrorT $ val)

--}}}

-- Show ----------------------------------------------------------------------{{{
showVal :: LispVal -> String
showVal (String str) = surround '"' str
    where surround x ys = x : (ys ++ [x])
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List vals) = "(" ++ unwordsList vals ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func args varargs body env) = "(lambda (" ++ unwords (map show args) ++
    (case varargs of
          Nothing -> ""
          Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
    show = showVal
--}}}

-- Evaluation -----------------------------------------------------------------{{{
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
         Bool False -> eval env alt
         otherwise  -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _): body)) =
    makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
                                     Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                                     Nothing -> return env
apply (IOFunc func) args = func args
apply x _ = throwError $ NotFunction "expected function" (show x)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("=", numBoolBinop (==))
             , (">", numBoolBinop (>))
             , ("<", numBoolBinop (<))
             , ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eqv?", eqv)
             , ("eq?", eqv)
             , ("equal?", equal)
             ]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map makePrimitiveFunc primitives)
    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
          makeFunc con (var, func) = (var, con func)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               ]


-- IO operations -----------------------------------------------------------{{{

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc x = throwIOError $ NotFunction "not a function" (show x)

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ x = throwIOError $ NumArgs 1 x

throwIOError :: LispError -> IOThrowsError a
throwIOError = liftThrows . throwError

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort x = throwIOError $ NumArgs 1 x

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc x = throwIOError $ Default $ "write: unepected argument: " ++ show x

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc x = throwIOError $ Default $ "read unexpected argument " ++ show x

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = String <$> (liftIO $ readFile filename)
readContents x = throwIOError $ Default $ "readContents: unexpected argument: " ++ show x

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> (load filename)

--}}}

-- List operations ---------------------------------------------------------{{{

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- }}}

-- Equality operations -------------------------------------------------------{{{

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                                  Left err -> False
                                  Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    ((==) <$> unpacker arg1 <*> unpacker arg2 ) `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgs = throwError $ NumArgs 2 badArgs
--}}}

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpack op args = if length args /= 2
                              then throwError $ NumArgs 2 args
                              else Bool <$> (op <$> left <*> right)
                                where left = unpack first
                                      right = unpack second
                                      [first,second] = args

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool val = throwError $ TypeMismatch "boolean" val

unpackStr ::  LispVal -> ThrowsError String
unpackStr (String str) = return str
unpackStr val = throwError $ TypeMismatch "string" val

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                                    then throwError $ TypeMismatch "number" $ String n
                                    else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
--}}}

-- Variables -----------------------------------------------------------------{{{

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError a = ErrorT LispError IO a

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound env var = readIORef env >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar env var = do
    env <- liftIO $ readIORef env
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar env var val = do
    env <- liftIO $ readIORef env
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef val))
          (lookup var env)
    return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar env var val = do
    alreadyDefined <- liftIO $ isBound env var
    if alreadyDefined
       then setVar env var val >> return val
       else liftIO $ do
           valueRef <- newIORef val
           envRef <- readIORef env
           writeIORef env ((var, valueRef) : envRef)
           return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars env bindings = readIORef env >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBindings bindings)
          addBindings (var, val) = do ref <- newIORef val
                                      return (var, ref)

--}}}

-- REPL ---------------------------------------------------------------------{{{

readEval :: Env -> String -> IOThrowsError LispVal
readEval env x = liftThrows (readExpr x) >>= eval env

flushStr :: String -> IO ()
flushStr str = putStr str *> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt *> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString expr >>= putStrLn
    where evalString :: String -> IO String
          evalString expr = runIOThrows $ show <$> ((liftThrows $ readExpr expr) >>= eval env)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
       then return ()
       else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lithp>>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= putStrLn

main = do
    args <- getArgs
    if null args then runRepl else runOne $ args

--}}}

