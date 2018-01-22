ft_test_sign x = if (x < 0) then print "negatve" else print "positive"

main = do
    ft_test_sign 5
    ft_test_sign 0
    ft_test_sign (-42)