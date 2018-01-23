ft_print_comb () = print_nums 0 1 2 where
    print_nums x y z = do
        putStr $ show x
        putStr $ show y
        putStr $ show z
        if (x < 7) then do  
            putStr ", "
            if (z < 9) then print_nums x y (z + 1)
            else if (y < 8) then print_nums x (y + 1) (y + 2)
            else if (x < 7) then print_nums (x + 1) (x + 2) (x + 3)
            else do putStr ""
        else do putStr "\n"

main = do
    ft_print_comb ()