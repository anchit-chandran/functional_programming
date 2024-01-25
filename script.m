sum_n::num->num
sum_n 1 = 1
sum_n n = n + sum_n(n-1)

plus::(num,num)->num
plus (1, y) = 1 + y
plus (x, 1) = x + 1
plus (x, y) = 1 + 1 + plus(x-1, y-1)
















 
