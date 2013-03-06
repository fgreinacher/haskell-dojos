areLeapYears :: [Int] -> [(Int, Bool)]
areLeapYears xs = [(x, isLeapYear x)| x <- xs]

isLeapYear :: Int -> Bool
isLeapYear x = 
	x `isDividableBy` 4 && (not (x `isDividableBy` 100) || x `isDividableBy` 400)

isDividableBy :: Int -> Int -> Bool
isDividableBy x y = (mod x y) == 0
	
