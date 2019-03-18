ft_string_all :: (a -> Bool) -> [a] -> Bool
ft_string_all f s = all f s

main =
    let is_digit c = c >= '0' && c <= '9' in
    do
        print $ ft_string_all is_digit "9coucouc"
        print $ ft_string_all is_digit "0123456"
        print $ ft_string_all is_digit "coucou6"
        print $ ft_string_all is_digit ""