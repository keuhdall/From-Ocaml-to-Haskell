ft_countdown :: Int -> IO ()
ft_countdown x
    | x > 0 = do
        print x
        ft_countdown $ x-1
    | otherwise = print 0

main = do
    ft_countdown 5
    putStrLn "-----"
    ft_countdown 0
    putStrLn "-----"
    ft_countdown (-2)