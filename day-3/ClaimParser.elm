module ClaimParser exposing (Claim, fromString)

import Parser exposing (..)


type alias Claim =
    { id : Int
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


fromString : String -> Result String Claim
fromString =
    Parser.run parseClaim >> Result.mapError Parser.deadEndsToString


parseClaim : Parser Claim
parseClaim =
    -- "#1 @ 896,863: 29x19"
    succeed Claim
        |. symbol "#"
        |= int
        |. spaces
        |. symbol "@"
        |. spaces
        |= int
        |. symbol ","
        |= int
        |. symbol ":"
        |. spaces
        |= int
        |. symbol "x"
        |= int
