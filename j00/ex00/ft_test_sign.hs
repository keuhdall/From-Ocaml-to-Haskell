ft_test_sign x = if (x < 0) then putStrLn "negatve" else putStrLn "positive"

main = do
    ft_test_sign 5
    ft_test_sign 0
    ft_test_sign (-42)