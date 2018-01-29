tak x y z = tak_loop x y z where
    tak_loop x y z =
        if (x < y) then
            tak_loop (tak_loop (x - 1) y z) (tak_loop (y - 1) z x) (tak_loop (z - 1) x y)
        else z

main = do
    putStrLn $ show $ tak 1 2 3
    putStrLn $ show $ tak 5 23 7
    putStrLn $ show $ tak 9 1 0
    putStrLn $ show $ tak 1 1 1
    putStrLn $ show $ tak 0 42 0
    putStrLn $ show $ tak 23498 98734 98776