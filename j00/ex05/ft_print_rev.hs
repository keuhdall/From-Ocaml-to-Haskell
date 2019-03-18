ft_print_rev :: String -> IO ()
ft_print_rev s = putStrLn $ reverse s

main = do
    ft_print_rev "Hello World"
    ft_print_rev ""
    ft_print_rev "coucou"