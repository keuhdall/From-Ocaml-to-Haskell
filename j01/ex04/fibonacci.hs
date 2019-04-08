fibonacci :: Int -> Int
fibonacci n
    | n < 0  = -1
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci (n-2) + fibonacci (n-1)

main = do
    putStrLn $ show $ fibonacci (-42)
    putStrLn $ show $ fibonacci 1
    putStrLn $ show $ fibonacci 3
    putStrLn $ show $ fibonacci 6