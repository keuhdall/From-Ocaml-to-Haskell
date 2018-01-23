ft_print_rev s =
    let len = length s in
    print_char len where
        print_char i = if (i > 0) then do
            putChar $ s !! (i - 1)
            print_char (i - 1)
        else
            putStr "\n"

main = do
    ft_print_rev "Hello World"
    ft_print_rev ""
    ft_print_rev "coucou"