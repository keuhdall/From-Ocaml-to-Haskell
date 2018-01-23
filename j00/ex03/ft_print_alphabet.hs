import Data.Char

ft_print_alphabet () = print_char 'a' where
    print_char c =
        if (ord c <= ord 'z') then do
            putStr (c:[])
            print_char (chr ((ord c) + 1))
        else putStr "\n"

main =
    ft_print_alphabet ()