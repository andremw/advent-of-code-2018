module InventoryManagement exposing (findCommonLetters, hasNDuplicateLetters, hasOneCommonLetter)

import Dict exposing (..)


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


hasOneCommonLetter : String -> String -> Bool
hasOneCommonLetter firstString secondString =
    countDifferentLettersAtTheSamePosition firstString secondString == 1


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


findCommonLetters : List String -> String
findCommonLetters boxes =
    "fgij"
