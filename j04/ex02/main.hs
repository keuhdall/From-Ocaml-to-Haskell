import qualified Color
import qualified Value
import qualified Card

print_cards l = case l of
    (hd:tl) -> do
        putStrLn $ "getValue : " ++ (Value.toStringVerbose (Card.getValue hd)) ++ " ; getColor : " ++ (Color.toStringVerbose (Card.getColor hd))
        putStrLn $ "toString : " ++ (Card.toString hd) ++ " ; toStringVerbose : " ++ (Card.toStringVerbose hd)
        putStrLn "======"
        print_cards tl
    []      -> return ()

main = do
    let card2S = Card.newCard Value.T2 Color.Spade
        card10H = Card.newCard Value.T10 Color.Heart
        cardKS = Card.newCard Value.King Color.Spade
        cardQD = Card.newCard Value.Queen Color.Diamond
        cardAD = Card.newCard Value.As Color.Diamond
        cardAC = Card.newCard Value.As Color.Club
        cardList = [card2S, card10H, cardKS, cardQD, cardAD, cardAC] in do
            print_cards (Card.all)
            putStrLn "=== compare tests ==="
            putStrLn "compare 2S and 10H :"
            putStrLn $ show $ Card.compare card2S card10H
            putStrLn "compare KS and QD :"
            putStrLn $ show $ Card.compare cardKS cardQD
            putStrLn "compare AD and AC : "
            putStrLn $ show $ Card.compare cardAD cardAC
            putStrLn "=== max tests ==="
            putStrLn "max of KS and 10H : "
            putStrLn (Card.toStringVerbose (Card.max cardKS card10H))
            putStrLn "max of 2S and 10H : "
            putStrLn (Card.toStringVerbose (Card.max card2S card10H))
            putStrLn "max of AC and AD : "
            putStrLn (Card.toStringVerbose (Card.max cardAC cardAD))
            putStrLn "=== min tests ==="
            putStrLn "min of KS and 10H : "
            putStrLn (Card.toStringVerbose (Card.min cardKS card10H))
            putStrLn "min of 2S and 10H : "
            putStrLn (Card.toStringVerbose (Card.min card2S card10H))
            putStrLn "min of AC and AD : "
            putStrLn (Card.toStringVerbose (Card.min cardAC cardAD))
            putStrLn "=== best test ==="
            putStrLn (Card.toStringVerbose (Card.best cardList))
            putStrLn "=== isOf tests ===";
            putStrLn "is 10H of S ?"
            putStrLn $ show $ Card.isOf card10H Color.Spade
            putStrLn "is 10H of H ?"
            putStrLn $ show $ Card.isOf card10H Color.Heart
            putStrLn "=== isX tests ==="
            putStrLn "is 10H Spade ?"
            putStrLn $ show $ Card.isSpade card10H
            putStrLn "is 10H Heart ?"
            putStrLn $ show $ Card.isHeart card10H
            putStrLn "is 10H isDiamond ?"
            putStrLn $ show $ Card.isDiamond card10H
            putStrLn "is 10H Club ?"
            putStrLn $ show $ Card.isClub card10H