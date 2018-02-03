import qualified Color

print_list l = case l of
    (hd:tl) -> do
        putStrLn $ Color.toStringVerbose $ hd
        print_list tl
    []      -> putStr ""



main = do
    print_list $ Color.all