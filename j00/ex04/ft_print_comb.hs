ft_print_comb () = ft_print_comb' 0 1 2 where
    ft_print_comb' x y z
        | x < 7 = do
            print_nums x y z
            putStr ", "
            next_num x y z
        | otherwise = do
            print_nums x y z
            putStr "\n"
    next_num x y z
        | z < 9 = ft_print_comb' x y (z+1)
        | y < 8 = ft_print_comb' x (y+1) (y+2)
        | x < 7 = ft_print_comb' (x+1) (x+2) (x+3)
    print_nums x y z = putStr $ show x ++ show y ++ show z

main = do
    ft_print_comb ()