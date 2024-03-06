|| Q2a) --------
|| [*] can take 2 values:
||     1. The empty list `[]`
||     2. The value (of type `*`) cons'd to a list containing values of the same type `[*]`
||         That is, (* : [*])

|| Q2b) --------
|| The function `f` attempts to take in a list of nums and keep only values which are 1 or 2.
|| However, there are 2 issues:
||     1) The pattern `f ('1' : rest) = 1 : (f rest)` is invalid because list elements can only be of the same type (in this case, it seems of type `num`). Therefore, there cannot a '1' - it must be `1` as a num.
||     2) The base case, `f [] = 0`, is incorrect as we cannot cons an element to an element (`0`), only a list. This must instead be `f [] = []`.


|| Q2c) --------
numchar ::= Num num | Char char
|| Q2d) --------
numchartree ::= Tnil | Tree (numchartree, numchar, numchartree)

|| Q2e) --------
|| `insertnum` should take a num and insert it into a `numchartree`, in a sorted manner (L < R).
|| The easiest way is to simply inserting at a Tnil node.
|| More difficult is inserting at a Tree node. In this case, we must insert the num, and set either its ltree or rtree equal to the replaced Tree node.
|| Test case 1 ==
|| insertnum (Num 2) (Tree (Tnil, (Num 1), Tnil)) => (Tree (Tnil, (Num 1), (Tree (Tnil, (Num 2), Tnil))))
|| insertnum (Num 2) (Tree (ltree, val, insertnum (Num 2) Tnil))
|| insertnum (Num 2) (Tree (ltree, val, insertnum (Num 2) Tnil))

|| Test case 2 ==
|| insertnum (Num 1) (Tree (Tnil, (Num 2), Tnil)) => (Tree ((Tree (Tnil, (Num 1, Tnil))), (Num 2), Tnil))

insertnum :: numchar -> numchartree -> numchartree
insertnum item Tnil || base case -> simply insert node at tnil
    = Tree (Tnil, item, Tnil)
insertnum item (Tree (ltree, val, rtree))
    = (Tree (insertnum item ltree, val, rtree)), 
        if item < val || the ordering will be (<);; if True, place left tree
    = (Tree (ltree, val, insertnum item rtree)),
        otherwise

