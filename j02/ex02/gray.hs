print_int n = putStr $ show n

concat_lst l1 l2 = case l1 of
    (hd:tl) -> hd:(concat_lst tl l2)
    []      -> l2

reverse_lst l = rev_loop l [] where
    rev_loop l new = case l of
        (hd:tl) -> rev_loop tl (concat_lst [hd] new)
        []      -> new

prefix_lst prefix l =
    concat_lst [prefix] l

lst_map f l = map_loop l [] where
    map_loop l new = case l of
        (hd:tl) -> map_loop tl (concat_lst new [f hd])
        []      -> new

lst_cover :: (f -> a) -> l -> ()
lst_cover f l = case l of
    (hd:tl) -> do
        f hd
        lst_cover f tl
    []      -> ()

print_lst f seq = case seq of
    (hd:sc:tl)  -> do
        f print_int head
        putStr " "
        print_lst f (sc:tl)
    (hd:tl)     -> do
        f print_int hd
        print_lst f tl
    []          -> ()

gray n = gray_loop n [[]] where
    gray_loop n seq_ret =
        if (n <= 0) then seq_ret
        else gray_loop (n - 1) (concat_lst (lst_map (prefix_lst 0) seq_ret) (lst_map
            (prefix_lst 1) (reverse_lst seq_ret)))

main = do
    print_lst $ lst_cover (gray 42)
{-main = do
    putStrLn "Test with [3] : "
    print_lst $ gray 3
    putStrLn "\nTest with [2] : "
    print_lst $ gray 2
    putStrLn "\nTest with [1] : "
    print_lst $ gray 1
    putStrLn "Test with [5] : "
    print_lst $ gray 5-}