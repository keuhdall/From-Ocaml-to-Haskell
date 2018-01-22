ft_power n pow = 
    if (n == 0) then 0
    else do
        if (pow == 0) then 1
        else n * (ft_power n (pow - 1))

main = do
    print $ (ft_power 5 5)
    print $ (ft_power 0 5)
    print $ (ft_power 5 0)
    print $ (ft_power 3 2)