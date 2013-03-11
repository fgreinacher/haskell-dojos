{-|
Develop a recently-used-list class to hold strings 
uniquely in Last-In-First-Out order.

o) A recently-used-list is initially empty.

o) The most recently added item is first, the least
   recently added item is last.

o) Items can be looked up by index, which counts from zero.

o) Items in the list are unique, so duplicate insertions
   are moved rather than added.

Optional extras

o) Null insertions (empty strings) are not allowed.

o) A bounded capacity can be specified, so there is an upper
   limit to the number of items contained, with the least
   recently added items dropped on overflow.
-}

module Mru where
    
data Capacity = Unbounded | BoundedTo Int 
    deriving Show

data MruList a = MruList { 
    storage :: [a], 
    capacity :: Capacity 
} deriving Show

-- Creates a MRU list with the specified capacity.
createUnbounded :: (Eq a) => MruList a
createUnbounded = MruList [] Unbounded
     
-- Creates a MRU list with the specified capacity.
createBoundedTo :: (Eq a) => Int -> MruList a
createBoundedTo capacity 
     | capacity <= 0    = error "Capacity must be greater than zero."
     | otherwise        = MruList [] (BoundedTo capacity)
                
-- Adds the specified item to the specified MRU list.
add :: (Eq a) => MruList a -> a -> MruList a
add (MruList storage Unbounded) item = MruList (newStorage storage item) Unbounded
add (MruList storage capacity@(BoundedTo limit)) item = MruList (take limit $ (newStorage storage item)) capacity

newStorage :: (Eq a) => [a] -> a -> [a]
newStorage storage item = (item : (filter (/= item) storage))

-- Gets the item at the specified index.
lookup :: (Eq a) => MruList a -> Int -> a
lookup mruList index = (storage mruList) !! index


