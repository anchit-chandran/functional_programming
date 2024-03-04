||  applylist [(+ 10),(* 3)] 2 will evaluate to [12,6].

applylist :: [* -> **] -> * -> [**]

applylist [] x 
    = []
applylist (fn : rest_fns) x 
    = ((fn x) : (applylist rest_fns x))
