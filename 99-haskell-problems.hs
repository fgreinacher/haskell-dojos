-- http://www.haskell.org/haskellwiki/99_questions/1_to_10

-- 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

-- *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: Eq a => [a] -> [[a]]
pack xs = [xs]

-- 8
-- Eliminate consecutive duplicates of list elements.

compress :: Eq a => [a] -> [a]
compress = foldr (\x acc -> if(length acc > 0 && x == head acc) then acc else x:acc) []

-- 7
-- Flatten a nested list structure.

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldr (\x acc -> flatten x ++ acc) [] xs 

-- 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []     = False
isPalindrome (x:[]) = True
isPalindrome xs = (take (start) xs) == (reverse (drop (end) xs))
    where 
        start = floor middle
        end = ceiling middle
        middle = ((fromIntegral len) / 2.0)
        len = length xs        

-- 5
-- Reverse a list.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


-- 4
-- Find the number of elements of a list.
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- 3
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt _ 0 = error "Not found"
elementAt [x] 1 = x
elementAt (x:xs) 1 = x
elementAt (x:xs) k = elementAt xs (k-1)

-- 2
butLast' :: [a] -> [a]
butLast' [] = error "List must not be empty!"
butLast' [x] = []
butLast' (x:xs) = x : butLast' xs

-- 1 
last' :: [a] -> a
last' [] = error "List must not be empty!"
last' [x] = x
last' (_:xs) = last' xs

