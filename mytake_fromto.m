|| fromto will return the list starting at index `start` and inclusively finish at index `end`
fromto :: (num, num, [*]) -> [*]
fromto (start, end, a_list) =  chop_end(end - start, findstart(start, a_list)) || chop_end is called with `end-start` because findstart() will return a list that is `start` elements shorter 

|| chop_end will discard all elements after nth index inclusive and return the list
chop_end :: (num, [*]) -> [*]
chop_end(0, x : xs) = [x]
chop_end(n, x : xs) = x : chop_end(n-1, xs)

|| find start will discard n elements from the start of the list, returning a list
findstart :: (num, [*]) -> [*]
findstart(0, a_list) = a_list
findstart (n, to_throw : a_list) = findstart(n-1, a_list)

|| --------------------------------------------------------- MYTAKE -------------------------------------
|| mytake will return the first n items of the list, discarding the rest
mytake::(num, [*])->[*]

mytake(1, item : a_list ) = [item]	 	|| base case -> just take last item, discard rest of list
mytake(n, item : a_list) = item : mytake(n-1, a_list) 


















 
