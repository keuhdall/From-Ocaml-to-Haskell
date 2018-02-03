import qualified Color

print_list :: [a] -> ()
print_list l = case l of
    (hd:tl) -> do
        putStrLn hd
        print_list tl
    []      -> ()



main = do
    print_list Color.toStringVerbose (Color.all)