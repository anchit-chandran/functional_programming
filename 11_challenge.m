|| write a fn which takes input num[] (containing only 0,1,2, with at least one 0) and returns 3-tuple containing:
|| 1. number of 1s before first 0
|| 2. number of 2s before first 0
|| 3. length of longest sequence of 1s before first 0
|| e.g. 
    || count_pre_zero ([1,1,2,2,1,1,1,2,1,0,1,2,0]) = (6, 3, 3)
    || count_pre_zero ([0]) = (0,0,0)
    || count_pre_zero ([1,1,1,2,2,2,1,2,2,1,0,1,1,1,1,1,1,1,2]) = (5, 5, 3)

|| -------------- ONE PASS O(N) -------------
count_pre_zero :: [num] -> (num,num,num)
count_pre_zero xs = xcount_pre_zero (xs, 0,0,0,0)

xcount_pre_zero :: ([num], num, num, num, num) -> (num,num,num)
xcount_pre_zero (x : xs, count_1s, count_2s, curr_longest, max_longest) 
                                                                = (count_1s, count_2s, larger_num(curr_longest, max_longest)), if x=0 || STOP ITERATION -> return counts and max subseq
                                                                = xcount_pre_zero(xs, 1 + count_1s, count_2s, 1 + curr_longest, max_longest), if x=1 || increment 1s_count and curr_longest 1s subseq
                                                                = xcount_pre_zero(xs, count_1s, 1 + count_2s, 0, larger_num(curr_longest, max_longest)), otherwise || only other case is 2; increment 2s_count, reset current longest 1s_subeq, set max_1s_subseq to the max(curr_longest, max_longest)




|| -------------- NAIVE O(3N) ---------
naive_count_pre_zero :: [num] -> (num,num,num)
naive_count_pre_zero xs = (count_nums_before_0(1, xs),count_nums_before_0(2, xs), longest_subseq_before_0(xs))

count_nums_before_0 :: (num, [num]) -> num
count_nums_before_0 (n, 0 : xs) = 0 || base case is zero as guaranteed to find this before empty list
count_nums_before_0 (n, n : xs) = 1 + count_nums_before_0(n, xs)
count_nums_before_0 (n, x : xs) = count_nums_before_0(n, xs)

|| longest subsequence of 1s before first 0
longest_subseq_before_0 :: [num] -> num
longest_subseq_before_0 xs = x_long_subseq_before_0(xs, 0, 0)
                             where
                             x_long_subseq_before_0(x : xs, curr_longest, max_longest) = larger_num(curr_longest, max_longest), if x=0
                                                                                       = x_long_subseq_before_0(xs, curr_longest+1, max_longest), if x=1
                                                                                       = x_long_subseq_before_0(xs, 0, larger_num(curr_longest, max_longest)), otherwise

larger_num :: (num,num) -> num
larger_num (a,b) = a, if a > b
               = b, otherwise