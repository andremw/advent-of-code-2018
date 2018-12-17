module SquareInches exposing (calculateSquareInches, createTuplesForClaim)

import ClaimParser exposing (Claim)
import Dict exposing (..)


calculateSquareInches : List String -> Int
calculateSquareInches claims =
    let
        {- here we filterMap removing the possible Err from the Results
           to get plain Claim records
        -}
        parsedClaims : List Claim
        parsedClaims =
            List.filterMap (ClaimParser.fromString >> Result.toMaybe) claims

        -- _ =
        --     Debug.log "parsed claims" parsedClaims
        tuples =
            List.concatMap createTuplesForClaim parsedClaims

        -- _ =
        --     Debug.log "tuples" tuples
        allTuples =
            List.foldl tupleToDict Dict.empty tuples

        _ =
            Debug.log "all tuples" allTuples
    in
    Dict.values allTuples |> List.filter (\n -> n >= 2) |> List.length


createTuplesForClaim : Claim -> List ( Int, Int )
createTuplesForClaim claim =
    let
        range =
            List.range 1

        firstRange =
            range claim.width

        secondRange =
            range claim.height

        createTuple dx dy =
            ( claim.x + dx, claim.y + dy )
    in
    firstRange
        |> List.concatMap (\rangeX -> secondRange |> List.map (\rangeY -> createTuple rangeX rangeY))


tupleToDict : ( Int, Int ) -> Dict ( Int, Int ) Int -> Dict ( Int, Int ) Int
tupleToDict claimTuple tuplesDict =
    if Dict.member claimTuple tuplesDict then
        Dict.update claimTuple (Maybe.map ((+) 1)) tuplesDict

    else
        Dict.insert claimTuple 1 tuplesDict
