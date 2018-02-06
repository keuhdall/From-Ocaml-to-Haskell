module Card(T(), newCard, allSpades, allHearts, allDiamonds, allClubs, all, getValue, getColor, toString,
    toStringVerbose, compare, max, min, best, isOf, isSpade, isHeart, isDiamond, isClub) where
    import Prelude hiding (all, max, min, compare)
    import qualified Color
    import qualified Value

    data T = T {
        color :: Color.T,
        value :: Value.T
    }

    newCard newColor newValue = T { color=newColor, value=newValue }

    allSpades :: [T]
    allSpades = fillAllSpades [] Value.T2 where
        fillAllSpades :: [T] -> Value.T -> [T]
        fillAllSpades l t =
            if (t == Value.As) then l ++ [(T { color=Color.Spade, value=Value.As })]
            else do
                l ++ [(T { color=Color.Spade, value=t})]
                fillAllSpades l (Value.next t)

    allHearts :: [T]
    allHearts = fillAllHearts [] Value.T2 where
        fillAllHearts l t =
            if (t == Value.As) then l ++ [(T { color=Color.Heart, value=Value.As })]
            else do
                l ++ [(T { color=Color.Heart, value=t})]
                fillAllHearts l (Value.next t)

    allDiamonds :: [T]
    allDiamonds = fillAllDiamonds [] Value.T2 where
        fillAllDiamonds l t =
            if (t == Value.As) then l ++ [(T { color=Color.Diamond, value=Value.As })]
            else do
                l ++ [(T { color=Color.Diamond, value=t})]
                fillAllDiamonds l (Value.next t)

    allClubs :: [T]
    allClubs = fillAllClubs [] Value.T2 where
        fillAllClubs l t =
            if (t == Value.As) then l ++ [(T { color=Color.Club, value=Value.As })]
            else do
                l ++ [(T { color=Color.Club, value=t})]
                fillAllClubs l (Value.next t)
    
    all = allSpades ++ allHearts ++ allDiamonds ++ allClubs

    getColor (T color _) = color
    getValue (T _ value) = value

    toString t = (Value.toString (value t)) ++ (Color.toString (color t))
    toStringVerbose t = "Card(" ++ (Value.toStringVerbose (value t)) ++ ", " ++ (Color.toStringVerbose (color t)) ++ ")"

    compare t1 t2
        |   (Value.toInt (value t1) < Value.toInt (value t2)) = (-1)
        |   (Value.toInt (value t1) > Value.toInt (value t2)) = 1
        |   otherwise = 0

    min t1 t2
        |   (Value.toInt (value t1) > Value.toInt (value t2)) = t2
        |   otherwise = t1

    max t1 t2
        |   (Value.toInt (value t1) >= Value.toInt (value t2)) = t1
        |   otherwise = t2
    
    best l = case l of
        (hd:tl) ->  foldl max hd tl
        []      ->  error "Error"
    
    isOf t c
        |   (color t) == c  = True
        |   otherwise       = False

    isSpade     t = isOf t Color.Spade
    isHeart     t = isOf t Color.Heart
    isDiamond   t = isOf t Color.Diamond
    isClub      t = isOf t Color.Club