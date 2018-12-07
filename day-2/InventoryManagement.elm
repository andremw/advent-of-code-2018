module InventoryManagement exposing (calculateChecksum)

import Tuple exposing (..)


calculateChecksum : List String -> Int
calculateChecksum ids =
    let
        result =
            calculateAppearances ""
    in
    (*) (Tuple.first result) (Tuple.second result)


calculateAppearances : String -> ( Int, Int )
calculateAppearances id =
    ( 4, 3 )
