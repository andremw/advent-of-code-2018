module InventoryManagement exposing (hasNDuplicateLetters)

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
