ft_countdown :: Int -> IO ()
ft_countdown x
    | x > 0 = do
        mapM_ (putStr . show) [x, (x-1)..0]
        putStr "\n"
    | otherwise = print 0

main = do
    ft_countdown 5
    putStrLn "-----"
    ft_countdown 0
    putStrLn "-----"
    ft_countdown (-2)