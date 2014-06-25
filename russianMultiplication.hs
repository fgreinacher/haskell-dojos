module RussianMultiplication where
  mul :: Int -> Int -> Int
  mul 1 y = y
  mul x y
    | even x    = next
    | otherwise = y + next
    where next = mul (x `div` 2) (y * 2)
