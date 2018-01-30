encode l = parse_list l 0 [] where
    parse_list l i acc = case l of
        (hd:sc:tl)  -> if (hd == sc) then
                parse_list (sc:tl) (i + 1) acc
            else
                parse_list (sc:tl) 0 (acc ++ [((i + 1), hd)])
        (hd:hl)     -> parse_list [] 0 (acc ++ [((i + 1), hd)])
        []          -> acc

print_val_int (i, n) = do
    putStr $ show i 
    putStr " ; "
    putStrLn $ show n

print_val_str (i, s) = do
    putStr $ show i
    putStr " ; "
    putStrLn s

print_list_int l = case l of
    (hd:tl) -> do
        print_val_int hd
        print_list_int tl
    []      -> putStrLn "====="

print_list_str l = case l of
    (hd:tl) -> do
        print_val_str hd
        print_list_str tl
    []      -> putStrLn "====="

main = do
    print_list_int (encode (1:2:2:3:3:3:[]));
    print_list_str (encode ("1":"2":"2":"3":"3":"3":[]))