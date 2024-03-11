|| Q2a ---
suit ::= Hearts | Spades | Clubs | Diamonds
card == (num, suit)

|| Q2b ---

|| e.g. if deck had 4 els, n will start as 2. If 5, n also cast to 2.
first_half :: num -> [card] -> [card]
first_half 0 rest = []
first_half n (front : rest) = (front : first_half (n-1) rest)

second_half :: num -> [card] -> bool -> [card]
second_half 0 any drop_last 
    = init any, if drop_last
    = any, otherwise
second_half n (front : rest) drop_last = second_half (n-1) rest drop_last

|| This function takes in [card] and returns it split in half ([cards], [cards]). If odd number, discard last.
split_half :: [card] -> ( [card], [card] )
split_half(deck) 
    = (first_half half_index deck, second_half half_index deck drop_last)
        where 
        deck_len = #deck
        half_index = entier ((deck_len)/2) || cast float to int
        drop_last 
            = False, if (deck_len mod 2) = 0 || drop last element in odd lists
            = True, otherwise

|| interleave takes tuple of equal sized [card] and interleaves starting with first card of second deck
interleave :: ([card],[card]) -> [card]
interleave ( first_half, second_half ) 
    = xinterleave ( first_half, second_half ) True || start with second half, interleave
        where
        xinterleave ( first_half, [] ) any 
            = first_half
        xinterleave ( (f_first : f_rest), (s_first : s_rest) ) get_second
            = s_first : xinterleave ( (f_first : f_rest), (s_rest) ) False, if get_second
            = f_first : xinterleave ( (f_rest), (s_first : s_rest) ) True, otherwise

shuffle :: num -> [card] -> [card]
shuffle num_shuffles deck
    = error "Deck cannot be > 52 cards", if #deck > 52
    = shuffle (num_shuffles-1) (interleave (split_half(deck))), if num_shuffles > 1
    = interleave (split_half(deck)), otherwise

|| TESTS
test_deck_1 = [(1,Hearts), (2,Hearts), (3,Spades), (4,Spades)]
result_deck_1 = [(3,Spades), (1,Hearts), (4,Spades), (2,Hearts)]
test_even_1_shuffle = shuffle 1 test_deck_1 = result_deck_1

test_deck_2 = [(1,Hearts), (2,Hearts), (3,Spades), (4,Spades), (5,Spades)]
result_deck_2 = [(3,Spades), (1,Hearts), (4,Spades), (2,Hearts)]
test_odd_1_shuffle = shuffle 1 test_deck_2 = result_deck_2

test_deck_3 = [(1,Hearts), (2,Hearts), (3,Spades), (4,Spades)]
result_deck_3 = [(4,Spades), (3,Spades), (2,Hearts), (1,Hearts)]
test_shuffle_2 = shuffle 2 test_deck_3 = result_deck_3

|| Q2c ---
|| cast a str(int) to int
char_to_num :: char -> num
char_to_num c = code c - code '0'

|| take a stringified int e.g. "123" and sum digits resulting in e.g. 1+2+3 = 6
sum_digits :: [char] -> num
sum_digits [] = 0
sum_digits (digit_1 : rest) = char_to_num(digit_1) + sum_digits(rest)

answer :: num -> num -> num
answer a b
    = result, if len_digits = 1
    = sum_digits (show result), otherwise
        where
            mult_as_str = show (a*b)
            result = sum_digits mult_as_str
            len_digits = #(show result)