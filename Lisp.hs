{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ExistentialQuantification #-}

import                  Control.Applicative ((<$>), (*>), (<*), (<*>))
import                  Control.Monad (liftM)
import      "mtl"       Control.Monad.Error
import                  Data.IORef
import                  System.IO (hGetContents, stdin, hFlush, stdout)
import                  Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExprA "lisp" input of
                      Left err  -> throwError . Parser $ err
                      Right val -> return val
--}}}
--}}}

-- Error ----------------------------------------------------------------------{{{

showError :: LispError -> String
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (NotFunction message func) = message ++ ": " ++ func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch typ found) = "Type mismatch, Expected " ++ typ ++ " ; found " ++ show found
showError (BadSpecialForm str val) = "Bad form " ++ str ++ " ; " ++ show val
showError (UnboundVar str val) = "Unbound " ++ str ++ " ; " ++ val
showError (Default str) = str

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError ::  ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
    show = showVal
--}}}

-- Evaluation -----------------------------------------------------------------{{{
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
         Bool False -> eval alt
         otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

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

-- REPL ---------------------------------------------------------------------{{{

readEval :: String -> ThrowsError LispVal
readEval x = readExpr x >>= eval

flushStr :: String -> IO ()
flushStr str = putStr str *> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt *> getLine

evalAndPrint :: String -> IO ()
evalAndPrint expr = putStrLn $ evalString expr
    where evalString :: String -> String
          evalString expr = extractValue $ trapError (show <$> readEval expr)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
       then return ()
       else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main = runRepl

--}}}

-- Variables -----------------------------------------------------------------{{{

--}}}

