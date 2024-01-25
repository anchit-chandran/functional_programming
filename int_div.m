int_div::(num, num)->num
int_div (numer, denom) = xint_div(numer, denom, 0)
			 where
			 xint_div(0, denom, accum) = accum
			 xint_div(numer, denom, accum) = xint_div(numer - denom, denom, accum+1)










||20 /5

||xint_div(20, 5, 0)
||xint_div(15, 5, 1)
||xint_div(10, 5, 2)
||xint_div(5, 5, 3)
||xint_div(0, 5, 4) -> return 4

