ackermann m n = ackermann_loop m n where
    ackermann_loop m n =
        if (m == 0 && n >= 0) then n + 1
        else if (m > 0 && n == 0) then ackermann_loop (m - 1) 1
        else if (m > 0 && n > 0) then ackermann_loop (m - 1) (ackermann_loop m (n - 1))
        else -1

main = do
    putStrLn $ show $ ackermann (-1) 7
    putStrLn $ show $ ackermann 0 0
    putStrLn $ show $ ackermann 2 3
    putStrLn $ show $ ackermann 4 1