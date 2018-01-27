import Data.Char

ft_rot_n n s =
    map rot_char s where
        rot_char c =
            if (ord c >= ord 'a' && ord c <= ord 'z') then
                chr $ (ord c - ord 'a' + n) `mod` 26 + ord 'a'
            else if (ord c >= ord 'A' && ord c <= ord 'Z') then
                chr $ (ord c - ord 'A' + n) `mod` 26 + ord 'A'
            else
                c

main = do
    putStrLn $ ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz"
    putStrLn $ ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz"
    putStrLn $ ft_rot_n 42 "0123456789"
    putStrLn $ ft_rot_n 2 "OI2EAS67B9"
    putStrLn $ ft_rot_n 0 "Damned !"
    putStrLn $ ft_rot_n 42 ""
    putStrLn $ ft_rot_n 1 "NBzlk qnbjr !"