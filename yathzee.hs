-- Yahzee --
import Data.List

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

score roll Pair = scoreNOfAKind 2 roll
score roll ThreeOfAKind = scoreNOfAKind 3 roll
score roll FourOfAKind = scoreNOfAKind 4 roll

score [1,2,3,4,5] SmallStraight = 15
score _ SmallStraight = 0

score [2,3,4,5,6] LargeStraight = 20
score _ LargeStraight = 0

scoreNOfAKind :: Int -> [Int] -> Int
scoreNOfAKind n roll = maybe 0 (\x -> sumOfFirstN n x) firstGroup
	where firstGroup = find (\x -> length x >= n) (sortedGroups roll)

sumOfFirstN :: Int -> [Int] -> Int
sumOfFirstN 0 _ = 0
sumOfFirstN 1 (x:_) = x
sumOfFirstN n xs = head xs + sumOfFirstN (n - 1) (tail xs) 
		
sortedGroups :: [Int] -> [[Int]]
sortedGroups xs = group . reverse . sort $ xs

sumRoll :: [Int] -> (Int -> Bool) -> Int
sumRoll roll f = foldl (\acc x -> if(f x) then acc + x else acc) 0 roll
