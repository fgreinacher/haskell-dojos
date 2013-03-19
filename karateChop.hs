chop :: Int -> [Int] -> Int
chop needle haystack = maybe (-1) id (chopFrom 0 needle haystack)

chopFrom :: Int -> Int -> [Int] -> Maybe Int
chopFrom _ _ [] = Nothing
chopFrom offset needle haystack = case needleComparedToMiddleOfHaystack of
    EQ -> Just offsetPlusMiddle
    LT -> chopFrom offset needle haystackToMiddle
    GT -> chopFrom offsetPlusMiddle needle haystackFromMiddle
    where
        needleComparedToMiddleOfHaystack = (needle `compare` (haystack !! middle))
        offsetPlusMiddle = offset + middle
        haystackToMiddle = take middle haystack
        haystackFromMiddle = drop middle haystack
        middle = (length haystack) `div` 2