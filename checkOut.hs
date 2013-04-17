type Price = Double
type Item = Char

getPrice :: String -> Price
getPrice ("AAA" ++ rest) = 130 + getPrice rest
getPrice ("A"++rest) = 50 + getPrice rest
getPrice ("BB"++rest) = 45 + getPrice rest
getPrice ("B"++rest) = 30 + getPrice rest
getPrice ("C"++rest) = 20 + getPrice rest    
getPrice ("D"++rest) = 15 + getPrice rest    
getPrice [] = 0 
