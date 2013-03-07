zeckendorfSum :: Int -> [Int]
zeckendorfSum number = foldl (\acc x -> if(snd x) then (acc ++ [fst x]) else acc) [] (zeckendorfTuples number)

zeckendorfBinary :: Int -> String
zeckendorfBinary number = map 
        (\x -> if(snd x) then '1' else '0')
        (zeckendorfTuples number)

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

fibonacciNumbersUpTo :: Int -> [Int]
fibonacciNumbersUpTo upperBound = 
    reverse $ 
    takeWhile (upperBound >=) $ 
    map fibonacciNumberAt [0..] 

fibonacciNumberAt :: Int -> Int
fibonacciNumberAt index 
    | index < 0     = error "index must no be negative"
    | index == 0    = 1
    | index == 1    = 2
    | otherwise     = fibonacciNumberAt (index - 2) + fibonacciNumberAt (index - 1)