module Color (T(), all, toString, toStringVerbose) where
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