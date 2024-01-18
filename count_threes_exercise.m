count_threes::[num]->num
count_threes [] = 0
count_threes (x : rest) = 1 + (count_threes rest), if x = 3
		 	= 0 + (count_threes rest), if x != num
			= 0 + (count_threes rest), otherwise
