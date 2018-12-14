module InventoryManagement exposing (findCommonLetters, findTwoCorrectBoxIDs, hasCommonLetters, hasNDuplicateLetters)

import Dict exposing (..)



-- PART 1


hasNDuplicateLetters : Int -> String -> Bool
hasNDuplicateLetters n word =
    word
        |> String.toList
        |> List.foldl keepOccurrences Dict.empty
        |> Dict.filter (\key value -> value == n)
        |> Dict.isEmpty
        |> not


keepOccurrences : comparable -> Dict comparable Int -> Dict comparable Int
keepOccurrences item occurrences =
    if Dict.member item occurrences then
        Dict.update item (Maybe.map ((+) 1)) occurrences

    else
        Dict.insert item 1 occurrences



-- PART 2


hasCommonLetters : Int -> String -> String -> Bool
hasCommonLetters num firstString secondString =
    countDifferentLettersAtTheSamePosition firstString secondString == num


countDifferentLettersAtTheSamePosition : String -> String -> Int
countDifferentLettersAtTheSamePosition firstString secondString =
    let
        initialDifference =
            0
    in
    differentLettersHelper (String.toList firstString) (String.toList secondString) initialDifference


differentLettersHelper : List Char -> List Char -> Int -> Int
differentLettersHelper first second count =
    if count > 1 then
        count

    else
        case first of
            [] ->
                count

            head :: tail ->
                let
                    secondListHead =
                        Maybe.withDefault '*' (List.head second)

                    secondListTail =
                        Maybe.withDefault [] (List.tail second)

                    -- _ =
                    --     Debug.log "first list"
                    --         ("head: "
                    --             ++ String.fromChar head
                    --             ++ " tail: "
                    --             ++ String.fromList tail
                    --         )
                    -- _ =
                    --     Debug.log "secondList"
                    --         ("head: "
                    --             ++ String.fromChar secondListHead
                    --             ++ " tail: "
                    --             ++ String.fromList secondListTail
                    --         )
                    -- _ =
                    --     Debug.log "previous diff count" count
                in
                if head /= secondListHead then
                    differentLettersHelper tail secondListTail (count + 1)

                else
                    differentLettersHelper tail secondListTail count


findCommonLetters : ( String, String ) -> String
findCommonLetters ( a, b ) =
    List.map2 Tuple.pair (String.toList a) (String.toList b)
        |> List.filter (\( strA, strB ) -> strA == strB)
        |> List.map Tuple.first
        |> String.fromList


findTwoCorrectBoxIDs : List String -> Maybe ( String, String )
findTwoCorrectBoxIDs ids =
    let
        filteredIds =
            ids
                |> List.filter (\id -> List.any (hasCommonLetters 1 id) ids)

        -- _ =
        --     Debug.log "filteredIds" filteredIds
        result =
            case filteredIds of
                first :: second :: _ ->
                    Just ( first, second )

                _ ->
                    Nothing
    in
    result
