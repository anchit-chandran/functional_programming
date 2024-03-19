expr ::= Var char | App expr expr | Abs char expr


|| Assumes nsub is a curried function
|| 
nsub :: expr -> expr -> expr -> expr
nsub (Var x) (Var a) (Var b) 
    = Var b, if x=a
    = Var x, otherwise
nsub (App x e)    (Var a) (Var b) 
    = 
nsub (Abs el el2) (Var a) (Var b) =
nsub i j k = error "Invalid input"