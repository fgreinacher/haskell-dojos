import Data.List.Split

checkHaikus :: String -> [(Int,Int,Int,Bool)]
checkHaikus input = map checkHaiku $ lines input

checkHaiku :: String -> (Int,Int,Int,Bool)
checkHaiku input = if (length lines /= 3) then error "Invalid number of lines" else (syllablesOfLine0,syllablesOfLine1,syllablesOfLine2,isValidHaiku)
    where
        lines = splitOn "/" input 
        syllablesOfLine0 = syllables $ lines !! 0
        syllablesOfLine1 = syllables $ lines !! 1
        syllablesOfLine2 = syllables $ lines !! 2
        isValidHaiku = 
            syllablesOfLine0 == 5 && 
            syllablesOfLine1 == 7 && 
            syllablesOfLine2 == 5

-- returns whether the given character is a vowel
isVowel :: Char -> Bool
isVowel = flip elem ['a','e','i','o','u','y']

-- counts the syllables of the given input string
syllables :: String -> Int
syllables s = length $ vowels 
    where
        vowels = filter (==True) filteredVowelMap
        filteredVowelMap = filterAdjacentDups vowelMap
        vowelMap = map isVowel s
        
-- filters adjacent duplicates of the given input list
filterAdjacentDups :: Eq a => [a] -> [a]
filterAdjacentDups [] = []
filterAdjacentDups (x:[]) = [x]
filterAdjacentDups (x:xs) = 
    if(x == head xs) 
        then filterAdjacentDups xs
        else x : (filterAdjacentDups xs)