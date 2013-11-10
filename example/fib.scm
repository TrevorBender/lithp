(load "src/stdlib.scm")

; fib n = n'th fibonacci number
(define (fib n) (if (<= n 1) 1 (* n (fib (- n 1)))))

(define (main . args) (write (map fib args)))
;(write "Enter a number")
;(define num (read))
;(define f (fib num))
;(write "fib=")
;(write f)
;'()
