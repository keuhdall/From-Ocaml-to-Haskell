ft_countdown x =
    if (x > 0) then do
        print x
        ft_countdown (x - 1)
    else
        print 0

main = do
    ft_countdown 5
    print "-----"
    ft_countdown 0
    print "-----"
    ft_countdown (-2)
