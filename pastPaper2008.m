|| Start time 12:10

|| Q3a --- Write your own version of the Miranda built-in function map, using
|| foldr. You may also use any other functions (except map) that you think
|| are necessary. [10 marks]

|| We know `foldr` will take a list, as defined using the cons operator x : [y : [z : [] ]] (technically the cons operator is applied prefix, not infix, however this is simpler syntax) and:
    || 1. Replace all (:) with the provided dyadic fn
    || 2. Replace the final empty list [] with the default provided
|| Thus, for mymap, we feed into foldr's fn the composition of first applying the provided mymap fn, and then cons'ing the result i.e. ((:) . fn). This achieves the behaviour of applying a given function to a list element, and keeping the output as a mapped list of a single element, rather than a reduced single value from a list.
|| Then, we set foldr's default to [], the Identity element. This is because anylist : [] = anylist.

mymap :: (* -> **) -> [*] -> [**]
mymap fn xs
    = foldr ((:) . fn) [] xs 

test_1 = mymap (+1) [1,2,3] = [2,3,4]

|| Q3b --- Write two new definitions for the function member, each of which has
|| the same type ([*]->*->bool) and functionality as the original
|| function member, the first using foldr and the second using foldl in
|| the function body.

|| Comments on this function found in answer for Q3c
member_foldr :: [*] -> * -> bool
member_foldr xs y
    = foldr is_same_as_y_or False xs || essentially initialises with ((False OR y=n) OR ...rest)
        where
            is_same_as_y_or = (or . is_same_as_y) || is_same_as_y_or :: any -> bool -> bool
            or = (\/) || (*note overriding a std env definition)
            is_same_as_y = (=) y

test_3b_1 = member_foldr [1,2,3] 1 = True
test_3b_2 = member_foldr [1,2,3] 4 = False

|| Comments on this function found in answer for Q3c
member_foldl :: [*] -> * -> bool
member_foldl xs y
    = foldl is_same_as_y_or False xs
        where
            is_same_as_y_or anybool any = anybool \/ (any = y) || Note having to define scoped curried fn

|| Tests
test_3b_3 = member_foldl [1,2,3] 1 = True
test_3b_4 = member_foldl [1,2,3] 4 = False

all_3b_pass = and [test_3b_1,test_3b_2, test_3b_3, test_3b_4]

|| Q3c ---

|| member_foldr
|| Using the same foldr (:) replacement logic as previous, we now require a foldr input fn which first checks whether y is the same as the extracted element x, then logical OR'ing this result with the rest of the list. We use function composition to achieve this, defining the scoped function is_same_as_y_or :: any -> bool -> bool. 
||
|| member_foldl
|| For the foldl version, we cannot simply repeat the previous composition steps. This is because the resulting initial foldl statement would be (`...rest` is pseudocode which assumes the same recursive application on the remaining elements, resulting in a bool value):
||      (or . is_same_as_y) False y ...rest
||      = (\/) ((=y) False) (...rest)
|| The member check (`(=y) False`) is applied in the wrong order resulting in checking whether y = False THEN applying (y = False) \/ ...rest.
|| Therefore, we define a scoped function is_same_as_y_or :: bool -> any -> bool) resulting in desired behaviour:
||      is_same_as_y_or anybool any = anybool \/ (any = y)
||
|| foldl and foldr can only be used interchangeably when the provided function is (1) associative and (2) commutative, at least with its identity. Our function (\/) . (=y) MUST be applied first to the list element THEN the accumulator_bool, so does not hold requirements for interchanging.
||
|| Our default value is False in both cases, acting as the Identity element. This is because anybool \/ False = anybool. 

|| Q4a --- Give a Miranda algebraic type definition for a set of items that may be
|| empty or contain items. The new type should be polymorphic and the
|| definition should be recursive, so that the set may contain any number of
|| items.

|| NOTE: set is defined as a collection of unordered UNIQUE elements.
set * ::= Empty | Cons * (set *)

|| Q4b --- Provide function definitions (including their type definitions) for the
|| following functions that operate on the new set type. The functions must
|| ensure that there are never any duplicate items in a set.


|| takes an item and a set and returns True if the item is in the set
memberset :: * -> set * -> bool
memberset y (Cons any rest) || Prefer if statements here as fewer lines
    = True, if y = any
    = False, otherwise

|| takes a function “f” (of type *->bool) and a
|| set and returns a set containing only those items x
|| for which f x returns True.
filterset :: (* -> bool) -> set * -> set *
filterset predicate Empty 
    = Empty
filterset predicate (Cons any rest)
    = Cons any (filterset predicate rest), if predicate any = True
    = filterset predicate rest, otherwise || discard if predicate is False

|| returns an empty set
nullset :: set *
nullset = Empty

|| adds an item to a set
addset :: * -> set * -> set *
addset any any_set
    = Cons any (any_set), if (filterset (=any) any_set) = nullset 
    = error "Can't add duplicate element!", otherwise

|| removes an item from a set
|| if item does not exist, simply returns the same set
subtractset :: * -> set * -> set *
subtractset any any_set
    = filterset (is_not_equal_to_x) any_set
        where
            is_not_equal_to_x = (~) . (=any)

|| returns the intersection of two sets
|| iterates through every element in set_a -> cons it if in set b
intersect :: set * -> set * -> set *
intersect Empty set_b
    = Empty
intersect (Cons any rest) set_b
    = Cons any (intersect rest set_b), if memberset any set_b
    = intersect rest set_b, otherwise

|| returns the union of two sets
|| iterates through every element in set_b -> add it if not in set A
union :: set * -> set * -> set *
union set_a Empty
    = set_a
union set_a (Cons any rest)
    = union (add_any_to set_a) rest, if any_not_in_set_a
    = union set_a rest, otherwise
        where
            add_any_to = addset any
            any_not_in_set_a = ~(memberset any set_a)

|| returns all the items of a set, collected into a list
showset :: set * -> [*]
showset Empty
    = []
showset (Cons any rest)
    = any : showset rest


|| Tests
test_charset = Cons 'a' (Cons 'b' (Cons 'c' Empty))
test_diff_charset =  Cons 'd' (Cons 'e' (Cons 'f' Empty))
test_numset = Cons 1 (Cons 2 (Cons 3 Empty))

test_memberset_true = memberset 'a' test_charset = True
test_memberset_false = memberset 'x' test_charset = False

test_filterset_true = filterset (='a') test_charset = Cons 'a' Empty
test_filterset_false = filterset (='x') test_charset = Empty

test_nullset = nullset = Empty

test_addset_works = addset 'd' test_charset = Cons 'd' (Cons 'a' (Cons 'b' (Cons 'c' Empty)))
test_addset_errors = addset 'a' test_charset || ?can't test for error. Not included in test suite.

test_subtractset_exists = subtractset 'a' test_charset = Cons 'b' (Cons 'c' Empty)
test_subtractset_not_exists = subtractset 'd' test_charset = test_charset

test_intersect_no_intersection = intersect test_charset test_diff_charset = Empty
test_intersect_intersection = intersect (Cons 'a' Empty) test_charset = Cons 'a' Empty

test_union_has_union = union test_charset (Cons 'a' (Cons 'd' Empty)) = Cons 'd' (Cons 'a' (Cons 'b' (Cons 'c' Empty)))
test_union_no_union = union test_charset (Cons 'a' Empty) = test_charset

test_showset_charset = showset test_charset = ['a','b','c']
test_showset_numset = showset test_numset = [1,2,3]

all_tests_q4 = and [
                test_memberset_true, 
                test_memberset_false,
                test_filterset_true,
                test_filterset_false,
                test_nullset,
                test_addset_works,
                test_subtractset_exists,
                test_subtractset_not_exists,
                test_intersect_no_intersection,
                test_intersect_intersection,
                test_union_has_union,
                test_union_no_union,
                test_showset_charset,
                test_showset_numset
                ]