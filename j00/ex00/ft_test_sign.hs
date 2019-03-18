ft_test_sign :: Int -> IO ()
ft_test_sign x
    | x < 0 = putStrLn "negatve"
    | otherwise = putStrLn "positive"

main = do
    ft_test_sign 5
    ft_test_sign 0
    ft_test_sign (-42)