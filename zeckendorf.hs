import Data.List

zeckString :: Int -> String
zeckString n = map (\x -> if(snd x) then '1' else '0') (done $ zeck n)

zeck :: Int -> ZeckState
zeck n = finalState (ZeckState n (fibsUpTo n) [])

finalState :: ZeckState -> ZeckState
finalState state 
    | nextState' == state    = nextState'
    | otherwise              = finalState nextState'
    where
        nextState' = nextState state

data ZeckState = ZeckState { 
    rest :: Int, 
    open :: [Int],
    done :: [(Int, Bool)] 
} deriving (Show, Eq)

nextState :: ZeckState -> ZeckState
nextState state
    | null open'              = state
    | (head open') <= rest'   = ZeckState (rest' - (head open')) (tail open') ((done') ++ [((head open'), True)])
    | otherwise               = ZeckState rest' (tail open') ((done') ++ [((head open'), False)])
    where 
        rest' = (rest state)
        open' = (open state)
        done' = (done state)

fits :: Int -> Int -> Bool
fits number limit
    | number > limit    = False
    | otherwise         = True

fibsUpTo :: Int -> [Int]
fibsUpTo upperBound = reverse $ takeWhile (\x -> x <= upperBound) fibs

fibs :: [Int]
fibs = map fibAt [0..] 

fibAt :: Int -> Int
fibAt index 
    | index < 0     = error "index must no be negative"
    | index == 0    = 1
    | index == 1    = 2
    | otherwise     = fibAt (index - 2) + fibAt (index - 1)