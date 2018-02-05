module Card(T(), newCard, allSpades, allHearts, allDiamonds, allClubs, all, getValue, getColor, toString,
    toStringVerbose, compare, max, min, best, isOf, isSpade, isHeart, isDiamond, isClub) where
    import Prelude hiding (all, max, min)
    import qualified Color
    import qualified Value

    data T = T {
        color :: Color.T,
        value :: Value.T
    } -- deriving (Show)

    newCard newColor newValue = T { color=newColor, value=newValue }

    allSpades = fillAllSpades [] T2 where
        fillAllSpades l t =
            if (t == As) then l ++ [(T { color=Spade, value=As })]
            else do
                l ++ [(T { color=Spade, value=t})]
                fillAllSpades l Value.next(t)

    allHearts = fillAllHearts [] T2 where
        fillAllHearts l t =
            if (t == As) then l ++ [(T { color=Heart, value=As })]
            else do
                l ++ [(T { color=Heart, value=t})]
                fillAllHearts l Value.next(t)

    allDiamonds = fillAllDiamonds [] T2 where
        fillAllDiamonds l t =
            if (t == As) then l ++ [(T { color=Diamond, value=As })]
            else do
                l ++ [(T { color=Diamond, value=t})]
                fillAllDiamonds l Value.next(t)

    allClubs = fillAllClubs [] T2 where
        fillAllClubs l t =
            if (t == As) then l ++ [(T { color=Club, value=As })]
            else do
                l ++ [(T { color=Club, value=t})]
                fillAllClubs l Value.next(t)
    
    all = allSpades ++ allHearts ++ allDiamonds ++ allClubs

    getColor (T color _) = color
    getValue (T _ value) = value

    toString t = (Value.toString t.value) ++ (Color.toString t.color)
    toStringVerbose t = "Card(" ++ (Value.toStringVerbose t.value) ++ ", " ++ (Color.toStringVerbose t.color) ++ ")"

    compare t1 t2
        |   (Value.toInt (t1.value) < Value.toInt (t2.value)) = (-1)
        |   (Value.toInt (t1.value) > Value.toInt (t2.value)) = 1
        |   otherwise = 0

    min t1 t2
        |   (Value.toInt t1.value > Value.toInt t2.value) = t2
        |   otherwise = t1

    max t1 t2
        |   (Value.toInt t1.value >= Value.toInt t2.value) = t1
        |   otherwise = t2
    
    best l = case l of
        (hd:tl) ->  foldl max hd tl
        []      ->  error "Error"
    
    isOf t color
        |   (color t) == color  = True
        |   otherwise           = False

    isSpade     t = isOf t Spade
    isHeart     t = isOf t Heart
    isDiamond   t = isOf t Diamond
    isClub      t = isOf t Club