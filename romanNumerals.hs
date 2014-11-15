module RomanNumerals (intToRoman, romanToInt) where

	import Data.Char

	romanToInt :: String -> Int
	romanToInt x = convert (map toUpper x)

	intToRoman :: Int -> String
	intToRoman = stringFromRomanNumber . romanNumberFromInt

	convert :: String -> Int
	convert "" = 0

	convert ('I' : 'V' : rest) = 4 + (convert rest)
	convert ('I' : 'X' : rest) = 9 + (convert rest)
	convert ('I' : rest) = 1 + (convert rest)

	convert ('V' : rest) = 5 + (convert rest)

	convert ('X' : 'L' : rest) = 40 + (convert rest)
	convert ('X' : 'C' : rest) = 90 + (convert rest)
	convert ('X' : rest) = 10 + (convert rest)

	convert ('L' : rest) = 50 + (convert rest)

	convert ('C' : 'D' : rest) = 400 + (convert rest)
	convert ('C' : 'M' : rest) = 900 + (convert rest)
	convert ('C' : rest) = 100 + (convert rest)

	convert ('D' : rest) = 500 + (convert rest)
	convert ('M' : rest) = 1000 + (convert rest)

	convert x = error $ "Unknown character sequence '" ++ x ++ "''"

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
