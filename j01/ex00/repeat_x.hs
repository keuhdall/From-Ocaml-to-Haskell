repeat_x i = repeat_loop i "" where
    repeat_loop i s =
        if (i < 0) then "Error"
        else if (i == 0) then s
        else repeat_loop (i - 1) (s ++ "x")

main = do
    putStrLn $ repeat_x (-5)
    putStrLn $ repeat_x 0
    putStrLn $ repeat_x 5
