{-

Zeckendorf Number
-----------------

Just as numbers can be represented in a positional
notation as sums of multiples of the powers of ten
(decimal) or two (binary); all the positive integers
can be represented as the sum of one or zero times
the distinct members of the Fibonacci series.

Recall that the first six distinct Fibonacci numbers
are: 1, 2, 3, 5, 8, 13.
The decimal number eleven can be written as

  0*13 + 1*8 + 0*5 + 1*3 + 0*2 + 0*1
  
or 010100 in positional notation where the columns
represent multiplication by a particular member of the
sequence. Leading zeroes are dropped so that eleven
decimal becomes 10100.

10100 is not the only way to make eleven from the
Fibonacci numbers however;

  0*13 + 1*8 + 0*5 + 0*3 + 1*2 + 1*1
  
or 010011 would also represent decimal 11. For a true
Zeckendorf number there is the added restriction that
no two consecutive Fibonacci numbers can be used which
leads to the former unique solution.

Your task is to generate and show here a table of the
Zeckendorf number representations of the decimal numbers
zero to twenty, in order.

-}

-- Determines the zeckendorf binary representation that makes up the specified number.
zeckendorfBinary :: Int -> String
zeckendorfBinary number = map 
        (\x -> if(snd x) then '1' else '0')
        (zeckendorfTuples number)

-- Determines the zeckendorf tuples that make up the specified number 
zeckendorfTuples :: Int -> [(Int, Bool)]
zeckendorfTuples number = third finalState
    where
        third (_, _, c) = c
        finalState = resolveState (number, (fibonacciNumbersUpTo number), [])
        resolveState state@(rest, [], done) = state
        resolveState (rest, (headOpen:tailOpen), done)
            | headOpen <= rest   = resolveState $ (rest - headOpen, tailOpen, markDone True)
            | otherwise          = resolveState $ (rest, tailOpen, markDone False)
            where markDone val = done ++ [(headOpen, val)]

-- Gets the fibonacci numbers below the specified upper bound
fibonacciNumbersUpTo :: Int -> [Int]
fibonacciNumbersUpTo upperBound = 
    reverse $ 
    takeWhile (upperBound >=) $ 
    map fibonacciNumberAt [0..] 

-- Gets the fibonacci number at the specified index
fibonacciNumberAt :: Int -> Int
fibonacciNumberAt index 
    | index < 0     = error "index must no be negative"
    | index == 0    = 1
    | index == 1    = 2
    | otherwise     = fibonacciNumberAt (index - 2) + fibonacciNumberAt (index - 1)