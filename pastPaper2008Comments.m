|| Start time 12:10

|| Q3a --- Write your own version of the Miranda built-in function map, using
|| foldr. You may also use any other functions (except map) that you think
|| are necessary. [10 marks]

|| We know `foldr` will take a list, as defined using the cons operator x : [y : [z : [] ]] (technically the cons operator is applied prefix, not infix, however this is simpler syntax) and:

|| COMMENTS on line above: 
|| (i)  style - keep both code lines and comment lines to a reasonable width
|| (ii) error - although you can write x : [y,z]  you can't write x : [y : [z:[]]]
||              instead you should write x : (y : (z : []))

    || 1. Replace all (:) with the provided dyadic fn
    || 2. Replace the final empty list [] with the default provided
|| Thus, for mymap, we feed into foldr's fn the composition of first applying the provided mymap fn, and then cons'ing the result i.e. ((:) . fn). This achieves the behaviour of applying a given function to a list element, and keeping the output as a mapped list of a single element, rather than a reduced single value from a list.

|| COMMENTS on line above: 
|| (i)  style - keep both code lines and comment lines to a reasonable width
|| (ii) code - nice use of partial application

|| Then, we set foldr's default to [], the Identity element. This is because anylist : [] = anylist.

|| COMMENTS on line above: 
|| code/logic - no!  3:[] = [3], which is not the same as 3
||              Strangely, in this instance it's not really an identity element per se, 
||              but it is the value that must appear at the end of a constructed list and
||              in the context of the lectures I'm happy for you to call it the Identity element
||              (since that's the name I used when talking about interchangeability).

mymap :: (* -> **) -> [*] -> [**]
mymap fn xs
    = foldr ((:) . fn) [] xs 

test_1 = mymap (+1) [1,2,3] = [2,3,4]
|| COMMENTS on line above: 
|| Good test

|| Q3b --- Write two new definitions for the function member, each of which has
|| the same type ([*]->*->bool) and functionality as the original
|| function member, the first using foldr and the second using foldl in
|| the function body.

|| Comments on this function found in answer for Q3c
member_foldr :: [*] -> * -> bool
member_foldr xs y
    = foldr is_same_as_y_or False xs || essentially initialises with ((False OR y=n) OR ...rest)
      || COMMENTS on line above: 
      || Actually this is foldr so it is (... ((penultimateitem=y) \/ ((lastitem=y) \/ False))))
        where
        || COMMENTS on line above: 
        || Best style is to start the "w" of where directly under the "f" of foldr.
            is_same_as_y_or = (or . is_same_as_y) || is_same_as_y_or :: any -> bool -> bool
            || COMMENTS on line above: 
            || (1) Best style is to start the first character of first definition directly
            ||     under the "w" of "where"
            || (2) It's a shame Miranda doesn't support type definitions in where blocks.
            ||     Placing the type in a comment is very good style.
            ||     However, try putting the comment just above the function, as you
            ||     would normally (outside a where block).
            or = (\/) || (*note overriding a std env definition)
            || COMMENTS on line above: 
            || This is ok if the definition is very close to where it is used
            || However in practice programs grow and usage could become separated
            || from definition by many lines, at which point there could be confusion.
            is_same_as_y = (=) y
            || COMMENTS on line above: 
            || Or (=y) or (y=), all of which are equally valid...

test_3b_1 = member_foldr [1,2,3] 1 = True
test_3b_2 = member_foldr [1,2,3] 4 = False
|| COMMENTS on line above: 
|| Good tests

|| Comments on this function found in answer for Q3c
member_foldl :: [*] -> * -> bool
member_foldl xs y
    = foldl is_same_as_y_or False xs
      || COMMENTS on line above: 
      || And this is foldl so it is ((False \/ (firstitem=y)) \/ (seconditem=y)) ... )
        where
        || COMMENTS on line above: 
        || Best style is to start the "w" of where directly under the "f" of foldr.
            is_same_as_y_or anybool any = anybool \/ (any = y) || Note having to define scoped curried fn
            || COMMENTS on line above: 
            || (1) Best style is to start the first character of first definition directly
            ||     under the "w" of "where"
            || (2) Your comment makes the line too long - it would be better to put the
            ||     comment on the line above.  Also I presume the comment refers to the 
            ||     function using the free variable "y" - in which case the expression 
            ||     "having to" is a little odd - you don't "have to", but you have 
            ||     "chosen to" (which is fine).  However, the order of the parameters
            ||     in foldl does make it necessary to define the function with two parameters
            ||     so you can access the second parameter to test for equality.

|| Tests
test_3b_3 = member_foldl [1,2,3] 1 = True
test_3b_4 = member_foldl [1,2,3] 4 = False

all_3b_pass = and [test_3b_1,test_3b_2, test_3b_3, test_3b_4]
|| COMMENTS on line above: 
|| Good tests


|| Q3c ---

|| member_foldr
|| Using the same foldr (:) replacement logic as previous, we now require a foldr input fn which first checks whether y is the same as the extracted element x, then logical OR'ing this result with the rest of the list. We use function composition to achieve this, defining the scoped function is_same_as_y_or :: any -> bool -> bool. 
|| COMMENTS on line above: 
|| (1) This line is toooooooooooo looooooong
|| (2) This looks like a valid comment for 3(b)/foldr and should appear in 3(b)/foldr, not here.
||     However, I will concede that when coupled with the following text it is a "comparison"
||     of the two, so no penalty here (but perhaps some danger of not getting high marks in 3(b).
||
|| member_foldl
|| For the foldl version, we cannot simply repeat the previous composition steps. This is because the resulting initial foldl statement would be (`...rest` is pseudocode which assumes the same recursive application on the remaining elements, resulting in a bool value):
||      (or . is_same_as_y) False y ...rest
||      = (\/) ((=y) False) (...rest)
|| The member check (`(=y) False`) is applied in the wrong order resulting in checking whether y = False THEN applying (y = False) \/ ...rest.
|| Therefore, we define a scoped function is_same_as_y_or :: bool -> any -> bool) resulting in desired behaviour:
||      is_same_as_y_or anybool any = anybool \/ (any = y)
|| COMMENTS on lines above: 
|| (1) Many of them are toooooooooooo looooooong
|| (2) This looks like a valid comment for 3(b)/foldl and should appear in 3(b)/foldl, not here.
||     However, I will concede that when coupled with the preceding text it is a "comparison"
||     of the two, so no penalty here (but perhaps some danger of not getting high marks in 3(b).
||
|| foldl and foldr can only be used interchangeably when the provided function is (1) associative and (2) commutative, at least with its identity. Our function (\/) . (=y) MUST be applied first to the list element THEN the accumulator_bool, so does not hold requirements for interchanging.
|| COMMENTS on lines above: 
|| This line is toooooooooooo looooooong, but the answer is ok.
||
|| Our default value is False in both cases, acting as the Identity element. This is because anybool \/ False = anybool. 

|| COMMENTS on lines above: 
|| It would be good to make it clear where one question ends and the other starts.
|| e.g.
|| ==========================================

|| Q4a --- Give a Miranda algebraic type definition for a set of items that may be
|| empty or contain items. The new type should be polymorphic and the
|| definition should be recursive, so that the set may contain any number of
|| items.

|| NOTE: set is defined as a collection of unordered UNIQUE elements.
set * ::= Empty | Cons * (set *)
|| COMMENTS on lines above: 
|| It might be nicer to choose constructor names that don't look so much like a list
|| - e.g. you could say set * ::=  EmptySet | Set * (set *)

|| Q4b --- Provide function definitions (including their type definitions) for the
|| following functions that operate on the new set type. The functions must
|| ensure that there are never any duplicate items in a set.

|| takes an item and a set and returns True if the item is in the set
memberset :: * -> set * -> bool
memberset y (Cons any rest) || Prefer if statements here as fewer lines
    = True, if y = any
    = False, otherwise
|| COMMENTS on lines above: 
|| Comment above type definition is good
|| BUT you forgot to include a specification for the second argument being Empty
|| e.g.
|| memberset y Empty
||     = False
|| memberset y (Cons any rest) || Prefer if statements here as fewer lines
||     = True, if y = any
||     = False, otherwise
|| ALSO you forgot to check the rest of the set!  So....
|| memberset y Empty
||     = False
|| memberset y (Cons any rest) || Prefer if statements here as fewer lines
||     = True, if y = any
||     = memberset y rest, otherwise


|| takes a function “f” (of type *->bool) and a
|| set and returns a set containing only those items x
|| for which f x returns True.
filterset :: (* -> bool) -> set * -> set *
filterset predicate Empty 
    = Empty
filterset predicate (Cons any rest)
    = Cons any (filterset predicate rest), if predicate any = True
    = filterset predicate rest, otherwise || discard if predicate is False
|| COMMENTS on lines above: 
|| Comment above type definition is good
|| The code is ok BUT could be simpler (and NB I use paretheses around the test
|| This is not always necessary, but I find it to be good defensive style for conditionals):
|| filterset predicate Empty 
||     = Empty
|| filterset predicate (Cons any rest)
||     = Cons any (filterset predicate rest), if (predicate any)
||     = filterset predicate rest, otherwise || discard if predicate is False

|| returns an empty set
nullset :: set *
nullset = Empty
|| OK

|| adds an item to a set
addset :: * -> set * -> set *
addset any any_set
    = Cons any (any_set), if (filterset (=any) any_set) = nullset 
    = error "Can't add duplicate element!", otherwise
|| COMMENTS on lines above:
|| You need to have some comments to explain the chosen semantics
|| in relation to duplicates.  NB you could choose to return the same set if
|| you find a duplicate (it's what you would have returned if the value
|| hadn't already sexisted in the set)
|| Also, using (memberset any any_set) instead of filterset would probably 
|| be more efficient

|| removes an item from a set
|| if item does not exist, simply returns the same set
subtractset :: * -> set * -> set *
subtractset any any_set
    = filterset (is_not_equal_to_x) any_set
        where
            is_not_equal_to_x = (~) . (=any)
|| COMMENTS on lines above:
|| (1) Actually your function removes ALL occurrences of that value from the set
||     rather than the first such item - you should say that in your comments.
|| (2) Also, see previous comments about indenting "where" and where-block functions
|| (3) Miranda has a ~= operator so you could say filterset (~=any) any_set

|| returns the intersection of two sets
|| iterates through every element in set_a -> cons it if in set b
intersect :: set * -> set * -> set *
intersect Empty set_b
    = Empty
intersect (Cons any rest) set_b
    = Cons any (intersect rest set_b), if memberset any set_b
    = intersect rest set_b, otherwise
|| COMMENTS on lines above:
|| Good, but could be more efficient with another pattern definition:
|| intersect Empty set_b
||     = Empty
|| intersect set_a Empty 
||     = Empty
|| intersect (Cons any rest) set_b
||     = Cons any (intersect rest set_b), if memberset any set_b
||     = intersect rest set_b, otherwise

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
|| COMMENTS on lines above:
|| See earlier comments on indenting.
|| The code is ok.
|| There are also other ways to do this, such as
||     union set_a set_b = mkset (set_a ++ set_b)
|| (Though that wasn't an expected answer).

|| returns all the items of a set, collected into a list
showset :: set * -> [*]
showset Empty
    = []
showset (Cons any rest)
    = any : showset rest
|| COMMENTS on lines above:
|| Yes, easy isn't it?
|| Can you try writing a function to give a printable representation of a set?
|| (eg to print to the screen?)



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
