concat_list l1 l2 = case l1 of
    []      ->  l2
    hd:tl   ->  hd:(concat_list tl l2)

map_list l f = case l of
    []      ->  []
    hd:tl   ->  f hd:(map_list f tl)

rev_list l acc = case l of
    []      ->  acc
    hd:tl   ->  rev_list tl (hd:acc)

gray n =
    if (n <= 1) then "0":"1":[]
    else
        let g = gray (n - 1)
            concat0 x = x ++ "0"
            concat1 x = x ++ "1" in
                concat_list (map_list concat0 g) (map_list concat1 (rev_list [] g))

print_list l = case l of
    []      ->  putStr "\n"
    hd:tl   ->  putStr hd

main = do
    print_list $ gray 1
    print_list $ gray 5