import qualified Color
import qualified Value
import qualified Card

print_cards l = case l of
    (hd:tl) -> do
        putStrLn "getValue : " ++ Card.Value.toStringVerbose (Card.getValue first) ++ " ; getColor : " ++ Card.Color.toStringVerbose (Card.getColor first)
        putStrLn "toString : " ++ (Card.toString first) ++ " ; toStringVerbose : " ++ (Card.toStringVerbose first)
        putStrLn "======"
        print_cards tl
    []      -> return ()

main = do
    let card2S = Card.newCard Card.Value.T2 Card.Color.Spade
        card10H = Card.newCard Card.Value.T10 Card.Color.Heart
        cardKS = Card.newCard Card.Value.King Card.Color.Spade
        cardQD = Card.newCard Card.Value.Queen Card.Color.Diamond
        cardAD = Card.newCard Card.Value.As Card.Color.Diamond
        cardAC = Card.newCard Card.Value.As Card.Color.Club
        cardList = [card2S, card10H, cardKS, cardQD, cardAD, cardAC]
    in
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
    putStrLn $ Card.isOf card10H Card.Color.Spade
    putStrLn "is 10H of H ?"
    putStrLn $ Card.isOf card10H Card.Color.Heart
    putStrLn "=== isX tests ==="
    putStrLn "is 10H Spade ?"
    putStrLn $ Card.isSpade card10H
    putStrLn "is 10H Heart ?"
    putStrLn $ Card.isHeart card10H
    putStrLn "is 10H isDiamond ?"
    putStrLn $ Card.isDiamond card10H
    putStrLn "is 10H Club ?"
    putStrLn $ Card.isClub card10H