module Color(T(), all, toString, toStringVerbose) where
    import Prelude hiding (all)

    data T = Spade | Heart | Diamond | Club

    all = [Spade, Heart, Diamond, Club]

    toString t = case t of
        Spade   ->  "S"
        Heart   ->  "H"
        Diamond ->  "D"
        Club    ->  "C"

    toStringVerbose t = case t of
        Spade   ->  "Spade"
        Heart   ->  "Heart"
        Diamond ->  "Diamond"
        Club    ->  "Club"


module Value (T(), all, toInt, toString, toStringVerbose, next, previous) where
    import Prelude hiding (all)
    data T = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

    all = [T2, T3, T4, T5, T6, T7, T8, T9, T10, Jack, Queen, King, As]

    toInt t = case t of
        T2      ->  1
        T3      ->  2
        T4      ->  3
        T5      ->  4
        T6      ->  5
        T7      ->  6
        T8      ->  7
        T9      ->  8
        T10     ->  9
        Jack    ->  10
        Queen   ->  11
        King    ->  12
        As      ->  13

    toString t = case t of
        T2      ->  "2"
        T3      ->  "3"
        T4      ->  "4"
        T5      ->  "5"
        T6      ->  "6"
        T7      ->  "7"
        T8      ->  "8"
        T9      ->  "9"
        T10     ->  "10"
        Jack    ->  "J"
        Queen   ->  "Q"
        King    ->  "K"
        As      ->  "A"

    toStringVerbose t = case t of
        T2      ->  "2"
        T3      ->  "3"
        T4      ->  "4"
        T5      ->  "5"
        T6      ->  "6"
        T7      ->  "7"
        T8      ->  "8"
        T9      ->  "9"
        T10     ->  "10"
        Jack    ->  "Jack"
        Queen   ->  "Queen"
        King    ->  "King"
        As      ->  "As"

    next t = case t of
        T2      ->  T3
        T3      ->  T4
        T4      ->  T5
        T5      ->  T6
        T6      ->  T7
        T7      ->  T8
        T8      ->  T9
        T9      ->  T10
        T10     ->  Jack
        Jack    ->  Queen
        Queen   ->  King
        King    ->  As
        As      ->  error "Error"

    previous t = case t of
        T2      ->  error "Error"
        T3      ->  T2
        T4      ->  T3
        T5      ->  T4
        T6      ->  T5
        T7      ->  T6
        T8      ->  T7
        T9      ->  T8
        T10     ->  T9
        Jack    ->  T10
        Queen   ->  Jack
        King    ->  Queen
        As      ->  King

module Card(T(), newCard, allSpades, allHearts, allDiamonds, allClubs, all, getValue, getColor, toString,
    toStringVerbose, compare, max, min, best, isOf, isSpade, isHeart, isDiamond, isClub) where
    import Prelude hiding (all)

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

    compare t1 t2 =
        |   (Value.toInt (t1.value) < Value.toInt (t2.value)) = (-1)
        |   (Value.toInt (t1.value) > Value.toInt (t2.value)) = 1
        |   otherwise = 0

    min t1 t2 =
        |   (Value.toInt t1.value > Value.toInt t2.value) = t2
        |   otherwise = t1

    max t1 t2 =
        |   (Value.toInt t1.value >= Value.toInt t2.value) = t1
        |   otherwise = t2
    
    best l = case l of
        (hd:tl) ->  foldl max hd tl
        []      ->  error "Error"
    
    isOf t color =
        |   (color t) == color  = True
        |   otherwise           = False

    isSpade     t = isOf t Spade
    isHeart     t = isOf t Heart
    isDiamond   t = isOf t Diamond
    isClub      t = isOf t Club