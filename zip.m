my_zip :: [*] -> [*] -> [[*]]
my_zip xs ys 
    = xmy_zip xs ys True
        where
            xmy_zip [] ys take_x
                = [ys]
            xmy_zip (x : xs) (y : ys) take_x 
                 = [x] : xmy_zip xs (y : ys) False, if take_x
                 = [y] : xmy_zip (x : xs) ys True, otherwise


