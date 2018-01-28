import Data.Maybe

repeat_string str i =
    repeat_loop i "" where
        repeat_loop i s =
            if (i < 0) then "Error"
            else if (i == 0) then s
            else repeat_loop (i - 1) (s ++ fromMaybe "x" str)

main = do
    putStrLn $ repeat_string Nothing 5
    putStrLn $ repeat_string (Just "coucou") 5
    putStrLn $ repeat_string Nothing 0
    putStrLn $ repeat_string (Just "coucou") 0
    putStrLn $ repeat_string Nothing (-1)
    putStrLn $ repeat_string (Just "coucou") (-1)