module SquareInches exposing (calculateSquareInches)

import ClaimParser exposing (Claim)


calculateSquareInches : List String -> Int
calculateSquareInches claims =
    let
        {- here we filterMap removing the possible Err from the Results
           to get the plain Claim records
        -}
        parsedClaims : List Claim
        parsedClaims =
            List.filterMap (ClaimParser.fromString >> Result.toMaybe) claims

        _ =
            Debug.log "parsed claims" parsedClaims
    in
    4
