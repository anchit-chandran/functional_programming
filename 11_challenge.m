|| write a fn which takes input num[] (containing only 0,1,2, with at least one 0) and returns 3-tuple containing:
|| 1. number of 1s before first 0
|| 2. number of 2s before first 0
|| 3. length of longest sequence of 1s before first 0
|| e.g. count_pre_zero ([1,1,2,2,1,2,1,0,1,2,0]) = (4, 3, 1)

|| Takes in a list of nums, returns count of 1s before first 0. E.g. count_1s([1,1,2,2,1,2,1,0,1,2,0]) = 4
count_1s :: [num] -> num
count_1s (1:xs) = 1 + count_1s(xs)
count_1s (0 : xs) = 0 || found a 0, stop iteration
count_1s (x:xs) = count_1s(xs)

