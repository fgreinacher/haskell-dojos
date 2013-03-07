{-|
Write a program that prints the numbers from 1 to 100.
But for multiples of three print "Fizz" instead of the
number and for the multiples of five print "Buzz". For
numbers which are multiples of both three and five
print "FizzBuzz".

Sample output:

1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
17
Fizz
19
Buzz
... etc up to 100
-}

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
    
