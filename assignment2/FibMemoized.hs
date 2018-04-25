module FibMemoized where

-- The Fibonacci numbers

-- Naive Fibonacci function

fib 0 = 0
fib 1 = 1
fib m = fib (m-2) + fib (m-1)

-- 32, 33 - seconds (show)

-- An algorithm which returns a pair of consecutive Fibonacci numbers.

fibP :: Int -> (Int,Int)

fibP 0 = (0,1)
fibP n = (y,x+y)
         where
         (x,y) = fibP (n-1)

-- The list of Fibonacci values, defined directly.

fibs ::[Integer]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
