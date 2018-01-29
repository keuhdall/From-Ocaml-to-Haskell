converges f x n =
    if (n < 0) then False
    else converges_loop f x n where
        converges_loop f x i =
            if (i == -1) then False
            else if (x == (f x)) then True
            else converges_loop f (f x) (i - 1)
  
main = do
    putStrLn $ show $ converges (( * ) 2) 2 3
    putStrLn $ show $ converges (\x -> x `div` 2) 2 3
    putStrLn $ show $ converges (\x -> x `div` 2) 2 2