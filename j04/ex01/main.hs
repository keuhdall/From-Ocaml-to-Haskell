import qualified Value

print_cards l = case l of
    (hd:tl) ->  do
                putStr $ show $ Value.toInt hd
                putStr " : "
                putStr $ Value.toString hd
                putStr " : "
                putStr $ Value.toStringVerbose hd
                putStr " next is : "
                putStr $ Value.toStringVerbose $ Value.next hd
                putStr " previous is : "
                putStrLn $ Value.toStringVerbose $ Value.previous hd
                print_cards tl
    []              -> return ()

main = print_cards (Value.all)