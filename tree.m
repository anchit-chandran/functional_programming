tree * ::= Tnil | Tree (tree *, *, tree *)

ordering * == (* -> * -> bool)
insertleaf :: ordering * -> tree * -> * -> tree *
insertleaf order Tnil item
    = Tree (Tnil, item, Tnil)
insertleaf order (Tree (ltree, node, rtree)) item
    = Tree (put ltree item, node, rtree),
        if order item node
    = Tree (ltree, node, put rtree item), otherwise
        where put = insertleaf order

list_to_tree :: ordering * -> [*] -> tree *
list_to_tree order
= foldl (insertleaf order) Tnil

map_tree :: (* -> *) -> tree * -> tree *
map_tree ff (Tnil) 
    = Tnil
map_tree ff (Tree (ltree, node, rtree)) 
    = Tree (map_tree ff (ltree), ff node, map_tree ff (rtree))

num_nodes :: tree * -> num
num_nodes Tnil 
    = 0
num_nodes (Tree (ltree, node, rtree))
    = 1 + num_nodes (ltree) + num_nodes (rtree)

a_tree = list_to_tree (>) [1,2,3,4] 