ft_is_palindrome s = check_char 0 where
    check_char i =
        if (i < length s `div` 2) then do
            if (s !! i == (s !! (length s - i - 1))) then
                check_char (i + 1)
            else False
        else True

main = do
    print $ ft_is_palindrome "radar"
    print $ ft_is_palindrome "madam"
    print $ ft_is_palindrome "aaaaaaaaaaaaaaaaaaaaaa"
    print $ ft_is_palindrome "aaaaaaaaaaaIaaaaaaaaaaa"
    print $ ft_is_palindrome "aaaaaaaaaaaaIaaaaaaaaaa"
    print $ ft_is_palindrome "kayak"
    print $ ft_is_palindrome "a"
    print $ ft_is_palindrome "aa"
    print $ ft_is_palindrome ""
    print $ ft_is_palindrome "blablabla"