|| Q1===============================================================Q1
|| The game called “Minefield” presents the user with a 10x10 grid of cells that are initially blank. Five (5) of these cells contain hidden mines. The user is invited to enter (x,y) coordinates (such that both x and y are between 1 and 10 inclusive) for cells that she wishes to visit. The user is given one point for every cell that is visited that does not contain a mine. As soon as the user visits a cell that contains a mine, or visits a cell that has already been visited, the game is over and the program prints the user’s score to the screen.

|| You are given the following type synonym definition for the game board:

|| >board == [[cell]]
|| Give an algebraic type definition for the type cell. 

|| A1=========A1

|| Given the board type synonym.
board == [[cell]]

|| Each cell can have a state of either 
|| - 'Safe' : an unvisited cell
|| - 'Mine' : a cell with a mine 
|| - 'Visited' : a cell already visited 
|| Hitting either 'Mine' or 'Visited' ends the game, 
|| as defined in the question.
cell ::= Safe | Mine | Visited

|| Q2===============================================================Q2
|| Give the definition (including its type) of the function init_board which takes as its argument a (possibly infinite) list of (x,y) coordinates representing the positions of the mines. Your function should generate a value of type board containing 95 empty cells and 5 cells containing mines in the appropriate positions.

|| A2===========A2
coords == (num, num) || Assumed each x and y is an int of inclusive range 1-9.

|| init_board first initialises a 100x100 board of Safe cells.
|| Then, for mine_coord in input [coords]:
||           iterate through board until coords match, returning the same
||           board but with current Safe replace with Mine.
|| We assume each mine_coord is a valid coords, with no duplicates.
|| NOTE: we could improve efficiency by removing the mine_coord from [coords]
|| once we assign the Mine.
|| init_board :: [coords] -> board
init_board mine_coords
      = insert_mines mine_coords safe_board 5
        where
        safe_board = create_safe_board 10

|| For each mine in the input (list of mine coords [(x1,y1), (x2,y2)...]), 
|| replace Safe cell in the input (100 x 100 [Safe] board) with a Mine, 
|| at that mine coord, up to 5 times. 
|| Returns the same board, but with 5 Mine cells.
insert_mines ::  [coords] -> board -> num -> board
insert_mines any_mines safe_board 0 || No more remaining mines, return board.
    = safe_board
insert_mines [] safe_board rem_mines || No more remaining mines, return board.
    = safe_board
insert_mines (mine_coords : rest) safe_board rem_mines
    = insert_mines rest (insert mine_coords safe_board) (rem_mines-1)

|| At given coords, replace the Safe cell with a Mine cell in a given board.
|| coords is (x,y); board is [ [Safe,Safe...], [Safe,Safe...], ...]
|| Function iterates through each row until finding correct one. Then, 
|| insert_mine_in_row iterates each col of that row, replacing the Safe cell,
|| with a Mine cell, returning the Row.
|| Technically, there should never be a case where we reach the end of the board
|| before finding a valid Mine coord, as we assume all mine coords are valid,
|| but we handle this case just to be safe.
insert :: coords -> board -> board
insert any_coord [] || technically should not occur
    = []
insert (1, y) (row : rest) || found row, insert mine, return row
    = (insert_mine_at y row) : rest
      where 
      || insert_mine_at :: num -> [cell] -> [cell]
      || Now that we have the correct row, iterate through to find the correct col 
      || to insert Mine.
      insert_mine_at 1 (a_cell : rest)
        = Mine : rest
      insert_mine_at any_col (a_cell : rest)
        = a_cell : (insert_mine_at (any_col-1) rest), if any_col > 1
        = error "Col search idx reached 0. Check work", otherwise
insert (x, y) (board_row : rest) || not matching, keep searching
    = board_row : (insert (x-1, y) rest)

|| Returns a board of len remaining_rows, where each item is row of [Safe]*10
create_safe_board :: num -> board
create_safe_board 1
    = [create_safe_row 10]
create_safe_board remaining_rows 
    = (create_safe_row 10) : create_safe_board (remaining_rows-1)

|| Returns row of [Safe]*10
create_safe_row :: num -> [cell]
create_safe_row 1 
    = [Safe]
create_safe_row remaining_cells
    = Safe : create_safe_row (remaining_cells-1)


|| TESTS
make_safe_board = init_board [(1,1), (2,1), (10, 10)]
test_create_safe_row = # create_safe_row 10 = 10
test_create_safe_board = # create_safe_board 10 = 10
test_init_board_len = # make_safe_board = 10
all_tests = and [
                test_create_safe_row,
                test_create_safe_board,
                test_init_board_len
            ]


|| Q3p1===============================================================Q3p1
|| Give the definitions (including types) of the following two functions:

|| — usermove. This function takes two arguments: the board and a single (x,y) coordinate. It returns a two-tuple containing (i) a boolean according to whether the user has hit a mine or a previously visited cell, and (ii) a new board, suitably updated to indicate which cells have been visited.

|| A3p1===========A3p1


|| usermove implements a less efficient but simple method (due to time constraints) - it utilises 2 functions for each
|| of the tuple's return values:
||  - check_cell_at, which takes in the requested play coords and the board, returns the
||             cell value.
||  - replace_cell_with, which takes in the play coords and board, returning the same board
||             but with that play cell replaced with either Visited or Mine.
|| Again we assume input play coords are valid for this game.
usermove :: board -> coords -> (bool, board) 
usermove curr_board play_coords 
    = (is_mine_or_visited, updated_board) 
      where
      is_mine_or_visited = (cell_value = Mine) \/ (cell_value = Visited)
      cell_value = check_cell_at play_coords curr_board
      updated_board 
        = replace_cell_with play_coords curr_board repl_cell_val
          where
          repl_cell_val 
            = Visited, if cell_value=Safe
            = cell_value, otherwise
      

|| First iterate through the rows, then col, then return value.
check_cell_at :: coords -> board -> cell
check_cell_at (0, col) any
    = error "Index error!"
check_cell_at (1, col) (curr_row : rest)
    = check_row col curr_row
      where
      || check_row :: num -> [cell] -> cell
      check_row 0 any 
        = error "Index error!"
      check_row 1 (curr_cell : rest)
        = curr_cell
      check_row col (curr_cell : rest)
        = check_row (col-1) rest
check_cell_at (any_row, col) (curr_row : rest)
    = check_cell_at (any_row-1, col) rest

|| First iterate through the rows, then col, then replace that cell with repl_cell. 
|| Finally, return the updated board. 
replace_cell_with :: coords -> board -> cell -> board
replace_cell_with (0, play_col) any repl_cell
    = error "Index error!"
replace_cell_with (1, play_col) (curr_row : rest) repl_cell
    = repl_in_col play_col curr_row repl_cell : rest
      where 
      || repl_in_col :: num -> [cell] -> cell -> [cell]
      repl_in_col 0 any_row repl_cell
        = error "Index error!"
      repl_in_col 1 (curr_cell : rest) repl_cell
        = repl_cell : rest
      repl_in_col play_col (curr_cell : rest) repl_cell
        = curr_cell : repl_in_col (play_col-1) rest repl_cell
replace_cell_with (play_row, play_col) (curr_row : rest) repl_cell
    = curr_row : replace_cell_with (play_row-1, play_col) rest repl_cell

|| TESTS
|| Create the game board with 5 mines
game_board = init_board [(1,1), (2,1), (3, 10), (4, 10), (10, 10)]
test_check_cell_at_mine1 = check_cell_at (1,1) game_board = Mine
test_check_cell_at_mine2 = check_cell_at (2,1) game_board = Mine
test_check_cell_at_safe1 = check_cell_at (1,2) game_board = Safe
test_check_cell_at_safe2 = check_cell_at (4,1) game_board = Safe
test_replace_cell_with1 = hd (replace_cell_with (1,2) game_board Visited)!1 = Visited

test_user_move_mine = fst (usermove game_board (1,1)) = True
test_user_move_visited 
    = fst (usermove board2 (1,2)) = True
      where 
      board2 = snd (usermove game_board (1,2))
test_user_move_safe = fst (usermove game_board (1,2)) = False

all_q3_tests = and [test_check_cell_at_mine1, 
                    test_check_cell_at_mine2, 
                    test_check_cell_at_safe1,
                    test_check_cell_at_safe2,
                    test_user_move_mine,
                    test_user_move_visited,
                    test_user_move_safe
                    ]


|| Q3p2===============================================================Q3p2
|| Give the definitions (including types) of the following two functions:

|| — showboard. this function takes a single argument: the board. It returns a list of characters with newlines embedded appropriately in order to display the board on the screen. Previously-visited cells should be shown as the character ’X’ and unvisited cells should be shown as the space character. Mines should be shown as spaces (i.e. hidden from the user). 

|| A3p2===========A3p2

showboard ::= board -> [char]
showboard []
    = ['']
showboard (curr_row : rest)
    = show_col curr_row ++ showboard rest

show_col :: [cell] -> [char]
show_col (Safe : rest)
    =
show_col (Visited : rest)
    = 
show_col (Mine : rest)
    = 