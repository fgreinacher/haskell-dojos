{-

Count Coins
-----------------

There are four types of common coins in US currency:
  quarters (25 cents)
  dimes (10 cents)
  nickels (5 cents) 
  pennies (1 cent)
  
There are 6 ways to make change for 15 cents:
  A dime and a nickel;
  A dime and 5 pennies;
  3 nickels;
  2 nickels and 5 pennies;
  A nickel and 10 pennies;
  15 pennies.
  
How many ways are there to make change for a dollar
using these common coins? (1 dollar = 100 cents).

[Source http://rosettacode.org]
-}

import Data.List

data Coin = Quarter | Dime | Nickel | Penny deriving (Eq, Show)	

valueOfCoin :: Coin -> Int
valueOfCoin Quarter = 25
valueOfCoin Dime = 10
valueOfCoin Nickel = 5
valueOfCoin Penny = 1

type CoinSet = (Int,Coin)

countCoins n = do
	let pcs 	= possibleCoinSets n
	putStrLn 	$ "There are " ++ (show $ length pcs) ++ " possible ways to make change for " ++ (show n) ++ " cents:"
	putStrLn 	$ printPossibleCoinSets pcs

join :: String -> [String] -> String
join i s = concat $ intersperse i s

printPossibleCoinSets :: [[CoinSet]] -> String
printPossibleCoinSets pcs = join ";\n" (map (\x -> join ", " $ map (\y -> printCoinSet y) x) $ pcs)

printCoinSet :: CoinSet -> String
printCoinSet (n,c)
	| c == Quarter	= printCoin n "quarter" "quarters"
	| c == Dime		= printCoin n "dime" "dimes"
	| c == Nickel	= printCoin n "nickel" "nickels"
	| c == Penny	= printCoin n "penny" "pennies"
	where
		printCoin n singular plural 
			| n == 0	= "no " ++ plural
			| n == 1	= "a " ++ singular
			| otherwise	= (show n) ++ " " ++ plural

valueOfCoinSet :: CoinSet -> Int
valueOfCoinSet coinSet = fst coinSet * (valueOfCoin $ snd coinSet)

possibleCoinSets :: Int -> [[CoinSet]]
possibleCoinSets n = filteredCoinSets
	where
		unfilteredCoinSets = [ 
			[quarterSets, dimeSets, nickelSets, pennySets] | 
			quarterSets <- (coinSets n Quarter),
			dimeSets <- (coinSets n Dime),
			nickelSets <- (coinSets n Nickel),  
			pennySets <- (coinSets n Penny),
			n == ((valueOfCoinSet quarterSets) + (valueOfCoinSet dimeSets) + (valueOfCoinSet nickelSets) + (valueOfCoinSet pennySets)) ]
		
		filteredCoinSets = map (\x -> (filter (\y -> (fst y) /= 0) x)) unfilteredCoinSets
	
coinSets :: Int -> Coin -> [CoinSet]
coinSets n c = 
	map (\x -> (x,c)) $ [0.. (n `div` (valueOfCoin c))]