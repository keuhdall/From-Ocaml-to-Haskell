ft_is_palindrome :: String -> Bool
ft_is_palindrome s
    | length s <= 1 = True
    | otherwise = ft_is_palindrome' s where
        ft_is_palindrome' s
            | even (length s) = let ss = splitAt (length s `div` 2) s in fst ss == snd ss
            | otherwise = let ss = splitAt (length s `div` 2) s in fst ss == drop 1 (snd ss)

main = do
    print $ ft_is_palindrome "radar"
    print $ ft_is_palindrome "madam"
    print $ ft_is_palindrome "aaaaaaaaaaaaaaaaaaaaaa"
    print $ ft_is_palindrome "aaaaaaaaaaaIaaaaaaaaaaa"
    print $ ft_is_palindrome "aaaaaaaaaaaaIaaaaaaaaaa"
    print $ ft_is_palindrome "kayak"
    print $ ft_is_palindrome "a"
    print $ ft_is_palindrome "aa"
    print $ ft_is_palindrome "ab"
    print $ ft_is_palindrome ""
    print $ ft_is_palindrome "blablabla"