ft_string_all f s = 
    apply 0 where
    apply n =
        if (n < length s) then do
            if (f (s !! n) == False) then False
            else apply (n + 1)
        else True


main =
    let is_digit c = c >= '0' && c <= '9' in
    do
        print $ ft_string_all is_digit "9coucouc"
        print $ ft_string_all is_digit "0123456"
        print $ ft_string_all is_digit "coucou6"
        print $ ft_string_all is_digit ""