bochvar ::= TRUE | FALSE | MEANINGLESS

bochvar_and :: bochvar -> bochvar -> bochvar
bochvar_and TRUE TRUE = TRUE 
bochvar_and TRUE FALSE = FALSE 
bochvar_and FALSE TRUE = FALSE
bochvar_and FALSE FALSE = FALSE
bochvar_and x y = MEANINGLESS