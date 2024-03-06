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
||  insertnum (Num 2) test_tree =>

|| Test case 2 ==
|| insertnum (Num 4) test_tree =

|| HELPER TO CREATE TREES FOR TESTING
insertleaf order Tnil item
    = Tree (Tnil, item, Tnil)
insertleaf order (Tree (ltree, node, rtree)) item
    = Tree (put ltree item, node, rtree),
        if order item node
    = Tree (ltree, node, put rtree item), otherwise
        where put = insertleaf order
list_to_tree order
= foldl (insertleaf order) Tnil

test_tree = list_to_tree (<) [Num 3, Num 2, Num 6,Num 5,Num 8, Num 9]

insertnum :: numchar -> numchartree -> numchartree
insertnum item Tnil || base case -> simply insert node at tnil
    = Tree (Tnil, item, Tnil)
insertnum item (Tree (ltree, val, rtree))
    = (Tree (insertnum item ltree, val, rtree)), 
        if item < val || the ordering will be (<);; if True, place left tree
    = (Tree (ltree, val, insertnum item rtree)),
        otherwise

|| 2f) ------
|| `deletenum` deletes a Num from numchartree, keeping it ordered (<), returning the numchartree.
|| Assuming no duplicates
|| easy case -> delete with 0-1 children; simply set parent node's child to current node's child

|| HELPER FN to find largest val in tree (which will be the rightmost node)
largest_val_ltree :: numchartree -> numchar
largest_val_ltree (Tree (ltree, val, Tnil))
    = val
largest_val_ltree (Tree (ltree, val, rtree))
    = largest_val_ltree rtree

deletenum :: numchar -> numchartree -> numchartree
deletenum (item) (Tree (Tnil, item, Tnil)) || easy case 1 -> found the item, no children
    = Tnil
deletenum (item) (Tree (ltree, item, Tnil)) || easy case 2 -> node has 1 child
    = ltree
deletenum (item) (Tree (Tnil, item, rtree)) || easy case 2 -> node has 1 child
    = rtree
deletenum (item) (Tree (ltree, item, rtree)) || 2 children -> replace with greatest node in left subtree
    = Tree (deletenum (get_largest_val_in_tree) ltree, get_largest_val_in_tree, rtree) || replace current val with highest val from left subtree, and delete that largest val node from ltree
        where get_largest_val_in_tree = largest_val_ltree ltree
deletenum (item) (Tree (ltree, val, rtree)) || Descend 
    = Tree (descend ltree, val, rtree), if item < val
    = Tree (ltree, val, descend rtree), otherwise
        where descend = deletenum (item)


|| Test case 1 - delete no child
test_2f_1 = deletenum (Num 2) test_tree = Tree (Tnil,Num 3,Tree (Tree (Tnil,Num 5,Tnil),Num 6,Tree (Tnil,Num 8,Tree (Tnil,Num 9,Tnil))))
|| Test case 2 - delete 1 child
test_2f_2 = deletenum (Num 8) test_tree = Tree (Tree (Tnil,Num 2,Tnil),Num 3,Tree (Tree (Tnil,Num 5,Tnil),Num 6,Tree (Tnil,Num 9,Tnil)))
|| Test case 2 - delete 2 child
delete_test_tree_2_child = list_to_tree (<) [Num 3, Num 2, Num 6,Num 5, Num 4, Num 8, Num 9]
test_2f_3 = deletenum (Num 6) delete_test_tree_2_child