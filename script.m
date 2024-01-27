|| mytake will return the first n items of the list, discarding the rest
mytake::(num, [*])->[*]

mytake(1, item : a_list ) = [item]	 	|| base case -> just take last item, discard rest of list
mytake(n, item : a_list) = item : mytake(n-1, a_list) 



















 
