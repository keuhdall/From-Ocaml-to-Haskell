print_lst l = case l of
    []      -> ()
    (hd:tl) -> do
        putStr (head ++ " ")
        print_lst tail

prepend c s =
    let s' = Bytes.create (String.length s + 1) in
    s'.[0] <- c;
    String.blit s 0 s' 1 (String.length s);
    s'

concat x y = case (x, y) of
    ([], [])    -> []
    ([], _)     -> y
    (_,[])      -> x
    (hd:tl, _)  -> hd:(concat tl y)

list_map f l = case l of
    []      -> []
    (hd:tl) -> (f hd):(list_map f tl)

gray n =
    if (n <= 1) then 
        ["0"; "1"]
    else
        do
            let g = gray (n - 1) in
            concat (list_map (prepend '0') g) (list_map (prepend '1') g)

main = do
    putStrLn "Test with [3] : "
    print_lst $ gray 3
    putStrLn "\nTest with [2] : "
    print_lst $ gray 2
    putStrLn "\nTest with [1] : "
    print_lst $ gray 1
    putStrLn "Test with [5] : "
    print_lst $ gray 5