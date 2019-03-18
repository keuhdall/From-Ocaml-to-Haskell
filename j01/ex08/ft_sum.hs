ft_sum :: (Float -> Float) -> Float -> Float -> Float
ft_sum f s e
    | s > e = 0
    | otherwise = ft_sum' f s e 0.0 where
        ft_sum' f s e acc
            | s == e+1 = acc
            | otherwise = ft_sum' f (s+1) e (acc + f s)

main = do
    putStrLn $ show $ ft_sum (\x -> x ** 2) (-10) (-1)
    putStrLn $ show $ ft_sum (\x -> x ** 2) 1 10
    putStrLn $ show $ ft_sum (\x -> x ** 2) 1 1
    putStrLn $ show $ ft_sum (\x -> x ** 2) 4 6
    putStrLn $ show $ ft_sum (\x -> x ** 2) 10 1