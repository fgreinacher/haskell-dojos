-- http://www.haskell.org/haskellwiki/99_questions/1_to_10

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

