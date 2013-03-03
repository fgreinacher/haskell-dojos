
spell :: Int -> String
spell number = spellPaddedString paddedString
	where 
		string = show number
		paddedString = padToMultipleOfThree string
        
spellPaddedString :: String -> String
spellPaddedString (x:y:z:[]) = spellThree x y z
spellPaddedString (x:y:z:x':y':z':[]) = spellThree x y z ++ " thousand, " ++ spellThree x' y' z'
spellPaddedString (x:y:z:x':y':z':x'':y'':z'':[]) = 
    spellThree x y z ++ " million, " ++ 
    spellThree x' y' z' ++ " thousand, " ++ 
    spellThree x'' y'' z''
		
padToMultipleOfThree :: String -> String
padToMultipleOfThree x 
    | missing == 0   = x
    | missing == 3   = x
    | otherwise     = (replicate missing '0') ++ x
    where missing = 3 - (length x `mod` 3)

spellThree '0' x y = spellTwo x y
spellThree x '0' '0' = spellOne x ++ " hundred"
spellThree x y z = spellOne x ++ " hundred and " ++ spellTwo y z
 
spellTwo '0' x = spellOne x
spellTwo '1' '0' = "ten"
spellTwo '1' '1' = "eleven"
spellTwo '1' '2' = "twelve"
spellTwo '1' '3' = "thirteen"
spellTwo '1' x = spellOne x ++ "teen"

spellTwo '2' x = spellTwoWithBase x "twenty"
spellTwo '3' x = spellTwoWithBase x "thirty"
spellTwo '4' x = spellTwoWithBase x "forty"
spellTwo '5' x = spellTwoWithBase x "fifty"
spellTwo '6' x = spellTwoWithBase x "sixety"
spellTwo '7' x = spellTwoWithBase x "seventy"
spellTwo '8' x = spellTwoWithBase x "eigthy"
spellTwo '9' x = spellTwoWithBase x "ninety"
	
spellTwoWithBase x base
	| x == '0' 	= base
	| otherwise = base ++ " " ++ spellOne x

spellOne '0' = "zero"
spellOne '1' = "one"
spellOne '2' = "two"
spellOne '3' = "three"
spellOne '4' = "four"
spellOne '5' = "five"
spellOne '6' = "six"
spellOne '7' = "seven"
spellOne '8' = "eight"
spellOne '9' = "nine"


