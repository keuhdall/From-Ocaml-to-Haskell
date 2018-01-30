crossover l1 l2 = check_lists l1 l2 [] where
    is_in_list l e = case l of
        (hd:tl) -> if (hd == e) then True else is_in_list tl e
        []      -> False
    check_lists l1 l2 acc = case l1 of
        (hd:tl) -> if (is_in_list l2 hd == True && is_in_list acc hd == False) then
                check_lists tl l2 (acc ++ [hd])
            else
                check_lists tl l2 acc
        []      -> acc
  
print_list_str l = case l of
    (hd:tl) -> do
        putStr (hd ++ " ")
        print_list_str tl
    []      -> putStr "\n"

print_list_int l = case l of
    (hd:tl) -> do
        putStr $ show hd
        putStr " "
        print_list_int tl
    []      -> putStr "\n"

main = do
    print_list_int (crossover (1:1:1:3:5:7:[]) (5:7:[]));
    print_list_str (crossover ("1":"1":"1":"3":"5":"7":[]) ("1":"1":"1":"2":[]))