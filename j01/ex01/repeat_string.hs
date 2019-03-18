repeat_string :: String -> Int -> String
repeat_string s i
    | i < 0 = "Error"
    | i == 0 = s
    | otherwise = [0..i] >> s

main = do
    putStrLn $ repeat_string "" 5
    putStrLn $ repeat_string "coucou" 5
    putStrLn $ repeat_string "" 0
    putStrLn $ repeat_string "coucou" 0
    putStrLn $ repeat_string "" (-1)
    putStrLn $ repeat_string "coucou" (-1)