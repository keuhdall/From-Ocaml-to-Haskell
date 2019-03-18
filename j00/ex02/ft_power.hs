ft_power :: Int -> Int -> Int
ft_power 0 _    = 0
ft_power _ 0    = 1
ft_power n pow  = n * ft_power n (pow-1)

main = do
    print $ ft_power 5 5
    print $ ft_power 0 5
    print $ ft_power 5 0
    print $ ft_power 3 2