plus :: [num] -> num
plus [] = 0
plus (x : xs) = x + plus(xs)
