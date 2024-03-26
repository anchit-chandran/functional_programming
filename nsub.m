|| ============================================= 
|| 1. (a) Give the syntax for the untyped lambda calculus without constants.

|| e :: x | e e | lambda x . e 
||      where 
        || x is a variable name, 
        || e is a lambda expression, 
        || e e is application, 
        || and lambda x . e is a lambda abstraction.

|| ============================================= 
|| (b) Give the Miranda code for an algebraic type called exp that defines the legal
|| values for an expression in the untyped lambda calculus without constants.

|| We assume that variable names can only be single characters, using the constructor `Var char`.
expr ::= Var char | App expr expr | Abs char expr

|| ============================================= 
|| (c) Give the Miranda code for a function called nsub that takes three arguments
|| (an expression of type exp and two variable names) and returns an expression
|| of type exp. The returned expression should be the same as the input
|| expression but with all free occurrences of the first variable name replaced by
|| the second variable name. 

nsub :: expr -> expr -> expr -> expr
|| We assume nsub to be a curried function taking its 3 parameters.
|| Instance where expression is simply a variable name.
|| The variable `x` will always be free, so perform a simple
|| check on whether `a=x` to replace.
nsub (Var x) (Var a) (Var b)
    = Var b, if a=x
    = Var x, otherwise

|| Instance where expression is an application of e_1 e_2.
|| We don't know whether there are free variables to replace yet, as the App
|| constructor just requires 2 `expr` parameters, so we
|| recursively call nsub on each `expr_`.
nsub (App expr_1 expr_2) (Var a) (Var b)
    = App (repeat_nsub expr_1) (repeat_nsub expr_2)
      where
      || repeat_nsub :: expr -> expr
      repeat_nsub any_expr = nsub any_expr (Var a) (Var b)

|| Instance where expression is an abstraction.
|| We define a lambda abstraction as being a `char` followed by some `expr`.
|| The `char` will always be bound, so cannot be replaced. However, there may
|| be free variables to replace within the provided `expr`.
nsub (Abs x expr_1) (Var a) (Var b)
    = Abs x expr_1

|| Catch-all error handling for any other input.
nsub any_expr any_var_1 any_var_2
    = error "Invalid input"


|| Tests
a = Var 'a'
b = Var 'b'
c = Var 'c'
x = Var 'x'

test_1_no_replace = nsub a b c = Var 'a'
test_1_should_replace = nsub a a c = Var 'c'

test_1_app1 = nsub (App a b) a b = App (Var 'b') (Var 'b')
test_2_app2 = nsub (App x a) a b = App (Var 'x') (Var 'b') 

test_1_abs1_all_b = nsub (Abs 'a' (App a a)) a b 
test_1_abs2_no_replace = nsub (Abs 'a' (App a a)) b x 

|| ============================================= (d) Give the Miranda code for a function called esub that takes three arguments
|| (an expression of type exp, a variable name, and another expression of type
|| exp) and returns an expression of type exp. The returned result should be
|| the same as the first input expression but with all free occurrences of the
|| variable name replaced by the second input expression. 
|| E.g. esub (App (Var 'b') (Var 'b')) (Var 'a') (App (Var 'b') (Var 'b'))

|| esub :: expr -> expr -> expr -> expr

|| || When input expression is a variable name, simply replace all free occurrences of that variable name with expr_2.
|| esub (Var x) (Var a) expr_2
||     = expr_2, if a=x 
||     = Var x, otherwise

|| || When input expression is an App, essentially return (with all relevant arguments): App esub(expr1) esub(expr2)
|| esub (App expr_x expr_y) (Var a) expr_2
||     = App (esub expr_x (Var a) expr_2) (esub expr_x (Var a) expr_2)

|| || When input expression is an Abs, first find esub(char) (let result of this = result), then return Abs result esub()
|| esub (Abs x expr_x) (Var x) expr_2 
||     = Abs x (esub expr_x (Var a) expr_2)

|| || Catch-all error handling for any other input
|| esub any_expr any_var any_expr2
||     = error "Invalid input"