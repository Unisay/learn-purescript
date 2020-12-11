module Homework.Recursion where

import Homework.Todo (todo')
import Prelude
import Debug.Trace

{- 5! == 5 * (4 * (3 * (2 * 1))) == 120

Test in REPL:

> factorialNonTailRecursive 5 
> 120

> factorialNonTailRecursive 120
> 0

> factorialNonTailRecursive 12000
> RangeError: Maximum call stack size exceeded

-}
factorialNonTailRecursive :: forall a. Eq a => Ring a => a -> a
factorialNonTailRecursive i = case spy "n" i of
  n
    | n == zero -> one
  n -> spy "a" $ n * factorialNonTailRecursive (sub n one)

{- Test in REPL:

> factorialTailRecursive 5
120

> factorialTailRecursive 120
0

> factorialTailRecursive 12000
0

-}
factorialTailRecursive :: Int -> Int
factorialTailRecursive = go 1
  where
  go :: Int -> Int -> Int
  go accum i = case spy "n" i of
    0 -> accum
    n -> go (spy "a" $ accum * n) (n - 1)

{- Test in REPL:

> factorialTailRecursive' 12000
0

> factorialTailRecursive' 12000.0
Infinity

> factorialTailRecursive' 1200.0
Infinity

> factorialTailRecursive' 120.0
6.689502913449135e+198
-}
{-

Another common pattern of computation is called tree recursion. 
As an example, consider computing the sequence of Fibonacci numbers, 
in which each number is the sum of the preceding two:

0, 1, 1, 2, 3, 5, 8, 13..

Test in REPL:
> fibonacciNonTailRecursive 1
1

> fibonacciNonTailRecursive 2
1

> fibonacciNonTailRecursive 3
2

> fibonacciNonTailRecursive 30
832040

> fibonacciNonTailRecursive 10000
^C -- Too long, aborted

-}
fibonacciNonTailRecursive :: Int -> Int
fibonacciNonTailRecursive fn = case fn of
  0 -> 0
  1 -> 1
  n -> (fibonacciNonTailRecursive (n - 1)) + (fibonacciNonTailRecursive (n - 2))

{- Test in REPL:

> fibonacciTailRecursive 0
0

> fibonacciTailRecursive 1
1

> fibonacciTailRecursive 100
-980107325

> fibonacciTailRecursive 100000000
1819143227

-}
fibonacciTailRecursive :: Int -> Int
fibonacciTailRecursive i = go 0 1 i
  where
  go :: Int -> Int -> Int -> Int
  go a b = case _ of
    0 -> a
    n -> go b (a + b) (n - 1)

{-

Дано натуральное число N. 
Выведите слово YES, если число N является точной степенью двойки, 
или слово NO в противном случае.

Операцией возведения в степень пользоваться нельзя!

> isPowerOfTwo 0
"NO"

> isPowerOfTwo 1
"YES"

> isPowerOfTwo 2
"YES"

> isPowerOfTwo 3
"NO"

> isPowerOfTwo 4
"YES"

> isPowerOfTwo 5
"NO"

> isPowerOfTwo 6
"NO"

> isPowerOfTwo 7
"NO"

> isPowerOfTwo 8
"YES"

> isPowerOfTwo 512
"YES"

> isPowerOfTwo 513
"NO"

-}
-- 0  1  2  3  4  5  6   7   8   9  10  11
-- 0  1  1  2  3  5  8  13  21  34  55  89 
-- a  b
isPowerOfTwo :: Int -> String
isPowerOfTwo i = go i 1
  where
  go :: Int -> Int -> String
  go x acc =
    if acc > x then
      "NO"
    else
      if acc == x then
        "YES"
      else
        go x (acc * 2)
