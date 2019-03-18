repeat_x :: Int -> String
repeat_x n
    | n < 0 = "Error"
    | n == 0 = ""
    | otherwise = [0..n] >> "x"

main = do
    putStrLn $ repeat_x (-5)
    putStrLn $ repeat_x 0
    putStrLn $ repeat_x 5
