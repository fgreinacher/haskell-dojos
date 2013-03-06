fizzBuzzKata :: String
fizzBuzzKata = show $ map (fizzBuzzStr) [1..100]  

fizzBuzzStr :: Int -> String
fizzBuzzStr x 
	| x `isDividableBy` 15 	= "FizzBuzz"
	| x `isDividableBy` 3 	= "Fizz"
	| x `isDividableBy` 5 	= "Buzz"
	| otherwise 			= show x
    
isDividableBy :: Int -> Int -> Bool
isDividableBy x y = (mod x y) == 0
    