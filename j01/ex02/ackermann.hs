ackermann :: Int -> Int -> Int
ackermann m n
    | m == 0 && n >= 0  = n+1
    | m > 0 && n == 0   = ackermann (m-1) 1
    | m > 0 && n > 0    = ackermann (m-1) (ackermann m (n-1))
    | otherwise         = -1

main = do
    putStrLn $ show $ ackermann (-1) 7
    putStrLn $ show $ ackermann 0 0
    putStrLn $ show $ ackermann 2 3
    putStrLn $ show $ ackermann 4 1