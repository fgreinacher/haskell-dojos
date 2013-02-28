  	
module Main where
	
main :: IO ()
main = putStrLn $ yahzeeKata

-- Yahzee --

yahzeeKata :: String
yahzeeKata = show $ score [6, 1, 1, 6, 6, 1] Sixes

data Category = 
	Ones | Twos | Threes | Fours | Fives | Sixes |
	Chance | Yahtzee | Pair | TwoPairs |
	ThreeOfAKind | FourOfAKind | SmallStraight | LargeStraight | FullHose
	deriving (Eq, Show)

score :: [Int] -> Category -> Int

score roll Ones = sumRoll roll (==1)
score roll Twos = sumRoll roll (==2)
score roll Threes = sumRoll roll (==3)
score roll Fours = sumRoll roll (==4)
score roll Fives = sumRoll roll (==5)
score roll Sixes = sumRoll roll (==6)

score roll Chance = sumRoll roll (\x -> True)
score roll Yahtzee = if(all (== (head roll)) (tail roll)) then 50 else 0

sumRoll :: [Int] -> (Int -> Bool) -> Int
sumRoll roll f = foldl (\acc x -> if(f x) then acc + x else acc) 0 roll
	
-- Number names --

romanNumberKata :: String
romanNumberKata = (show romanNumber) ++ "\n" ++ (stringFromRomanNumber romanNumber)
	where romanNumber = romanNumberFromInt 1234

data RomanNumber = RomanNumber { 
	thousands :: Int, 
	hundreds :: Int,
	tens :: Int,
	units :: Int
} deriving (Show)   

romanNumberFromInt :: Int -> RomanNumber
romanNumberFromInt x = RomanNumber thousands hundreds tens units
	where 
		thousands = (x `quot` 1000)
		thousandsValue = thousands * 1000
		hundreds = (x - thousandsValue) `quot` 100
		hundredsValue = hundreds * 100
		tens = (x - thousandsValue - hundredsValue) `quot` 10
		tensValue = tens * 10
		units = x - thousandsValue - hundredsValue - tensValue
		


stringFromRomanNumber :: RomanNumber -> String
stringFromRomanNumber romanNum = 
	(take (thousands romanNum) $ repeat 'M') ++ 
	(stringFromPart 4 $ hundreds romanNum) ++ 
	(stringFromPart 2 $ tens romanNum) ++ 
	(stringFromPart 0 $ units romanNum)

stringFromPart :: Int -> Int -> String
stringFromPart offset num
	| num <= 3 = take num $ repeat (chars !! offset)
	| num == 4 = [chars !! offset, chars !! (offset + 1)]
	| num <= 8 = [chars !! (offset + 1)] ++ (take (num - 5) $ repeat (chars !! offset))
	| num == 9 = [chars !! offset, chars !! (offset + 2)]
	
	where chars = ['I', 'V', 'X', 'L', 'C', 'D', 'M']

-- Leap years --

leapYearKata :: String
leapYearKata = show $ areLeapYears [1900, 1996, 2000, 2001]

areLeapYears :: [Int] -> [(Int, Bool)]
areLeapYears xs = [(x, isLeapYear x)| x <- xs]

isLeapYear :: Int -> Bool
isLeapYear x = 
	x `isDividableBy` 4 && (not (x `isDividableBy` 100) || x `isDividableBy` 400)

-- FizzBuzz --

fizzBuzzKata :: String
fizzBuzzKata = show $ map (fizzBuzzStr) [1..100]  

isDividableBy :: Int -> Int -> Bool
isDividableBy x y = (mod x y) == 0

fizzBuzzStr :: Int -> String
fizzBuzzStr x 
	| x `isDividableBy` 15 	= "FizzBuzz"
	| x `isDividableBy` 3 	= "Fizz"
	| x `isDividableBy` 5 	= "Buzz"
	| otherwise 			= show x
	
