;; Fibonacci
(fn (fib n)
    "(fib num)

Return the <num>th Fibonacci number."
    (select
     ((= n 0) 0)
     ((= n 1) 1)
     (otherwise (+ (fib (- n 1)) (fib (- n 2))))))
