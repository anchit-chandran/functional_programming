|| occurs is a function that returns the count of element `x` in a list 
|| occurs(2, [1,2,2,2,3,4]) = 3

occurs :: (*, [*]) -> num
occurs (x, []) = 0
occurs (x, x : rest) = 1 + occurs(x, rest)
occurs (x, y : rest) = occurs(x, rest)

|| occurs(2, [1,2,2,2,3,4])
|| = occurs(2, [2,2,2,3,4])
|| = 1 + occurs(2, [2,2,3,4])
|| = 1 + (1 + occurs(2, [2,3,4]))
|| = 1 + (1 + (1 + occurs(2, [3,4])))
|| = 1 + (1 + (1 + occurs(2, [4])))
|| = 1 + (1 + (1 + occurs(2, [])))
|| = 1 + (1 + (1 + 0))
