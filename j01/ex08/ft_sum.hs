ft_sum f s e =
    if (s > e) then (0/0) --Didn't find any other way to return null
    else loop f s e 0.0 where
        loop f s e acc =
            if (s == e + 1) then acc
            else loop f (s + 1) e (acc + f s)
  
main = do
    putStrLn $ show $ ft_sum (\x -> (x * x)) (-10) (-1)
    putStrLn $ show $ ft_sum (\x -> (x * x)) 1 10
    putStrLn $ show $ ft_sum (\x -> (x * x)) 1 1
    putStrLn $ show $ ft_sum (\x -> (x * x)) 4 6
    putStrLn $ show $ ft_sum (\x -> (x * x)) 10 1