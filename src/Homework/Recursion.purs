module Homework.Recursion where

import Homework.Todo (todo')

{- 5! == 5 * 4 * 3 * 2 * 1 == 120

Test in REPL:

> factorialNonTailRecursive 5 
> 120

> factorialNonTailRecursive 120
> 0

> factorialNonTailRecursive 12000
> RangeError: Maximum call stack size exceeded

-}
factorialNonTailRecursive :: Int -> Int
factorialNonTailRecursive =
  todo'
    "calculates factorial but uses stack \
    \so overflows on input values exceeding stack size (e.g. 12000)"

{- Test in REPL:

> factorialTailRecursive 5
120

> factorialTailRecursive 120
0

> factorialTailRecursive 12000
0

-}
factorialTailRecursive :: Int -> Int
factorialTailRecursive = todo' "calculates factorial without stack overflow"

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
factorialTailRecursive' :: forall a. a -> a
factorialTailRecursive' =
  todo'
    "calculates factorial without stack overflow, \
    \returns non-zero results for large factorials"

{-

Another common pattern of computation is called tree recursion. 
As an example, consider computing the sequence of Fibonacci numbers, 
in which each number is the sum of the preceding two:

0, 1, 1, 2, 3, 4, 5, 13, ..

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
fibonacciNonTailRecursive = todo' "please implement"

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
fibonacciTailRecursive = todo' "please implement"

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
isPowerOfTwo :: Int -> String
isPowerOfTwo = todo' "please implement. You can import and use Prelude.compare"
