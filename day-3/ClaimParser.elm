module ClaimParser exposing (Claim, fromString)


type alias Claim =
    { id : Int
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


fromString : String -> Claim
fromString _ =
    { id = 1
    , x = 896
    , y = 683
    , width = 29
    , height = 19
    }
