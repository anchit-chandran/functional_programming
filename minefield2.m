|| QUERIES
|| Does the cons operator require parentheses if you have multiple expressions? 
|| e.g. do you need to do 2)?
||          1) x : fn_b x y z ----> will this do `(x : fn_b) x y z`
||          2) x : (fn_b x y z)
|| Actual example:
||      ins_mine (m_row, m_col) (board_row : rest_board)
||           = board_row : (ins_mine ((m_row-1), m_col) rest_board)


|| Q1 ========================================================= Q1
|| The game called “Minefield” presents the user with a 10x10 grid of cells that are initially blank. Five (5) of these cells contain hidden mines. The user is invited to enter (x,y) coordsinates (such that both x and y are between 1 and 10 inclusive) for cells that she wishes to visit. The user is given one point for every cell that is visited that does not contain a mine. As soon as the user visits a cell that contains a mine, or visits a cell that has already been visited, the game is over and the program prints the user’s score to the screen.

|| You are given the following type synonym definition for the game board:

|| >board == [[cell]]
|| Give an algebraic type definition for the type cell. 

|| A1 ========================================================= A1

board == [[cell]]

|| Each cell's coordsinate logic/data is handled separate to the cell state.
|| We assume all coords are valid (each x,y is an int in inclusive range 1-9),
|| rather than specifying constructors for each of these ints e.g. 
||     One | Two | ... | Nine 
|| mainly because mathematical operations will be easier. Any obvious error
|| handling / input validation steps will be performed though.
coords == (num,num)
cell ::= Safe | Visited | Mine

|| Q2 ========================================================= Q2
|| Give the definition (including its type) of the function init_board which takes as its argument a (possibly infinite) list of (x,y) coordsinates representing the positions of the mines. Your function should generate a value of type board containing 95 empty cells and 5 cells containing mines in the appropriate positions.

|| A2 ========================================================= A2
|| init_board first creates a board of 100 Safe cells. It then iterates
|| through each mine in mine_coords_list, replacing the relevant Safe cell
|| in the 'safe board' with a Mine. It does this up to 5 times (as per q), even
|| if the supplied mine_coords_list is infinite. This is extensible by
|| adjusting the remaining parameter of insert_mines.
|| This less efficient approach (of creating a Safe board, then another board
|| with Mines) is used for better separation of concerns, allowing the creation
|| of more focussed functions which do fewer (ideally one) thing(s).
init_board :: [coords] -> board
init_board mine_coords_list
    =  insert_mines_into safe_board
       where
       safe_board = init_safe_board 10
       insert_mines_into = insert_mines mine_coords_list 5

|| Creates a list of dim (dimension) rows, where each row contains 
|| dim Safe cells. Uses simple recursion. Uses similar helper function
|| init_safe_cols.
init_safe_board :: num -> board
init_safe_board 0
    = []
init_safe_board dim
    = init_safe_cols 10 : init_safe_board (dim-1)

|| Takes in a dimension and returns [cell]*dim.
init_safe_cols :: num -> [cell]
init_safe_cols 0
    = []
init_safe_cols dim
    = Safe : init_safe_cols (dim-1)

|| Takes in a list of mine coords, and inserts num_mines mines into curr_board.
|| Approach grabs each mine, inserts into board, and feeds that new returned
|| board into recursive calls. 
|| Though we must create a new board for each mine, the mine list can be 
|| provided in any order - sacrificing efficiency for flexibility. If duplicate
|| mine coords are provided, this will throw an error (as assumed to be 
|| user mistake).
insert_mines :: [coords] -> num -> board -> board
insert_mines any_list 0 curr_board || Hit 5 mines inserted.
    = curr_board
insert_mines [] any_num_mines curr_board || Hit empty mine list.
    = curr_board
insert_mines (a_mine : rest) num_mines curr_board || Inserting mine 
    = insert_mines rest (num_mines-1) (ins_mine a_mine curr_board)

|| Inserts mine into board, returns new updated board.
|| Once a matching row is found, ins_row will return the row with Mine 
|| inserted.
ins_mine :: coords -> board -> board
ins_mine (1, col) (board_row : rest_board) || ROW MATCH, now col-wise search
    = (ins_row col board_row) : rest_board
      where 
      ins_row 1 (Mine : rest) || CELL MATCH but DUPLICATE mine
        = error "Duplicate mine coord provided."
      ins_row 1 (curr_cell : rest) || CELL MATCH - replace curr with Mine
        = Mine : rest 
      ins_row any_col (curr_cell : rest)|| Search step - decrement col until 1.
        = curr_cell : (ins_row (any_col-1) rest) 
ins_mine (m_row, m_col) (board_row : rest_board)
    = board_row : (ins_mine ((m_row-1), m_col) rest_board)



|| TESTS -----
dummy_mine_coords = [(1,1), (2,1), (10,10)]
dummy_board = init_board dummy_mine_coords

test_board_dims = (# dummy_board = 10) & (# (dummy_board!1) = 10)
test_dummy_board_mine = (dummy_board!0!0 = Mine) & (dummy_board!1!0 = Mine) &(dummy_board!9!9 = Mine)

all_tests = and [
                test_board_dims,
                test_dummy_board_mine
            ]