converges :: (Int -> Int) -> Int -> Int -> Bool
converges f x n
    | n < 0     = False
    | n == -1   = False
    | x == f x  = True
    | otherwise = converges f (f x) (n-1)
  
main = do
    putStrLn $ show $ converges (( * ) 2) 2 3
    putStrLn $ show $ converges (\x -> x `div` 2) 2 3
    putStrLn $ show $ converges (\x -> x `div` 2) 2 2