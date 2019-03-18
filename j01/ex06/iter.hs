iter :: (Int -> Int) -> Int -> Int -> Int
iter f x n
    | n < 0 = -1
    | n == 0 = x
    | otherwise = iter f (f x) (n-1)
  
main = do
    putStrLn $ show $ iter (\x -> x * x) 2 4
    putStrLn $ show $ iter (\x -> x * 2) 2 4
    putStrLn $ show $ iter (\x -> x * 2) 2 0
    putStrLn $ show $ iter (\x -> x * 2) 2 1
    putStrLn $ show $ iter (\x -> x * 2) 2 (-1)