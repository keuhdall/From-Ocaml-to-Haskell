iter f x n =
    if (n < 0) then -1
    else iter_loop f x n where
        iter_loop f x n =
            if (n == 0) then x
            else iter_loop f (f x) (n - 1)
  
main = do
    putStrLn $ show $ iter (\x -> x * x) 2 4
    putStrLn $ show $ iter (\x -> x * 2) 2 4
    putStrLn $ show $ iter (\x -> x * 2) 2 0
    putStrLn $ show $ iter (\x -> x * 2) 2 1
    putStrLn $ show $ iter (\x -> x * 2) 2 (-1)