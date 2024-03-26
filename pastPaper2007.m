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

|| If statements generally preferred within this function, over multiple pattern matches, 
|| because of reduced redundancy and clear visual reading.
nsub :: expr -> expr -> expr -> expr

|| Encounter variable name (which will be a char type). 
nsub (Var name) (Var to_replace) (Var replace_with)
    = Var to_replace, if to_replace = name
    = Var name, otherwise

|| Encounter application.
|| In this case we return the App constructor with nsub recursively called on each expression.
nsub (App expr1 expr2) (Var to_replace) (Var replace_with) || Pattern match only for error handling.
    = App (nsub expr1 (Var to_replace) (Var replace_with)) (nsub expr2 (Var to_replace) (Var replace_with))

|| Encounter abstraction. As we've defined an Abs type to only take a char (variable name) and some expr, 
|| all variables inside expr will be bound by the variable name. So we can simply return the same 
|| Abstraction expression unchanged.
nsub (Abs name any_expr) (Var to_replace) (Var replace_with)
    = Abs name any_expr

|| In all other cases, we will throw an error. We pattern match the (Var name) constructor
|| in all previous cases as this also enables us to throw errors in invalid inputs for
|| the replacement variable names.
nsub any_expr to_replace replace_with
    = error "Invalid input"

|| ============================================= 
|| (d) Give the Miranda code for a function called esub that takes three arguments
|| (an expression of type exp, a variable name, and another expression of type
|| exp) and returns an expression of type exp. The returned result should be
|| the same as the first input expression but with all free occurrences of the
|| variable name replaced by the second input expression. 

|| NOTE: similar to prev function, we pattern match the input replacement
|| vars, even when not required for that instance, as it enables error handling.
esub :: expr -> expr -> expr -> expr

|| Encounter variable name. Simply replace the entire (Var name) with expr.
esub (Var name) (Var to_repl) (Var repl_expr)
    = nsub (Var name) (Var to_repl) (Var repl_expr)

|| Encounter application. Return the App constructor with esub recursively called on each expression.
esub (App expr1 expr2) (Var to_repl) (Var repl_expr)
    = App (rpt_esub expr1 ) (rpt_esub expr2)
      where
      rpt_esub e = esub e (Var to_repl) (Var repl_expr)

|| Encounter abstraction. Return the Abs constructor itself as, by definition, there cannot be free
|| ocurrences of the Abs var_name, as discussed in the prev function.
esub (Abs var_name any_expr) (Var to_repl) (Var repl_expr)
    = Abs var_name any_expr

|| Catch all error handling
esub any_other_expr any_to_repl any_repl_expr
    = error "Invalid input."

|| ============================================= 
|| (e) Give the Miranda code for a function called beta that takes an expression of
|| type exp as input and returns an expression of type exp. The input
|| expression must be an application of a lambda definition to an argument, and
|| the returned expression should be the result of performing a simple beta
|| reduction on that application. Do not attempt to cater for free variable capture.

beta :: expr -> expr 

|| beta (Abs 'x' (Var 'x')) (Var 'y') = Var 'x'

beta (App (Abs var_name expr1) expr2)
    = esub expr1 (Var var_name) expr2 
