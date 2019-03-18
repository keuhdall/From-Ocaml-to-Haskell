tak :: Int -> Int -> Int -> Int
tak x y z 
    | x < y = tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
    | otherwise = z

main = do
    putStrLn $ show $ tak 1 2 3
    putStrLn $ show $ tak 5 23 7
    putStrLn $ show $ tak 9 1 0
    putStrLn $ show $ tak 1 1 1
    putStrLn $ show $ tak 0 42 0
    putStrLn $ show $ tak 23498 98734 98776