ft_countdown x =
    if (x > 0) then do
        putStrLn x
        ft_countdown (x - 1)
    else
        putStrLn 0

main = do
    ft_countdown 5
    putStrLn "-----"
    ft_countdown 0
    putStrLn "-----"
    ft_countdown (-2)
