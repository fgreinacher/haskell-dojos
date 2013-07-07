
checkHaiku :: String -> (Int,Int,Int,Bool)
checkHaiku input = (0,0,0,False)

vowels = ['a','e','i','o','u','y']

isVowel :: Char -> Bool
isVowel c = elem c vowels

countSyllables :: String -> Int
countSyllables s = length $ vowels 
    where
        vowels = filter (==True) filteredVowelMap
        filteredVowelMap = filterAdjacentDups vowelMap
        vowelMap = map isVowel s
        
filterAdjacentDups :: Eq a => [a] -> [a]
filterAdjacentDups [] = []
filterAdjacentDups (x:[]) = [x]
filterAdjacentDups (x:xs) = 
    if(x == head xs) 
        then filterAdjacentDups xs
        else x : (filterAdjacentDups xs)