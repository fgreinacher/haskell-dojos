
zeck :: Int -> [Char]
zeck n = "test"

reverseSmallerFibs :: Int -> [Int]
reverseSmallerFibs = reverse . smallerFibs

smallerFibs :: Int -> [Int]
smallerFibs n = takeWhile (\x -> x <= n) fibs

fibs :: [Int]
fibs = map nthFib [0..] 

nthFib :: Int -> Int
nthFib x 
    | x < 0     = error "x must no be negative"
    | x == 0    = 1
    | x == 1    = 2
    | otherwise = nthFib (x - 2) + nthFib (x - 1)