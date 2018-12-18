module AlchemicalReduction exposing (getRemainingUnits, getShortestPolymer)

import Set exposing (Set)


getRemainingUnits : String -> String
getRemainingUnits originalUnits =
    originalUnits
        |> String.toList
        |> List.foldl reduce []
        |> List.map String.fromChar
        |> List.reverse
        |> String.join ""


getShortestPolymer : String -> String
getShortestPolymer originalUnits =
    let
        -- reduce the amount of work by calling the solution from the previous exercise
        remainingUnits : String
        remainingUnits =
            getRemainingUnits originalUnits

        uniqueLowerChars : Set Char
        uniqueLowerChars =
            remainingUnits
                |> String.toList
                |> List.filter Char.isLower
                |> Set.fromList

        -- _ =
        --     Debug.log "unique lower chars" uniqueLowerChars
        unitsWithoutEachChar : List String
        unitsWithoutEachChar =
            uniqueLowerChars
                |> Set.toList
                |> List.map (\char -> getUnitsWithoutChar char (String.toList remainingUnits))

        -- _ =
        --     Debug.log "units without each char" unitsWithoutEachChar
        polymers : List String
        polymers =
            unitsWithoutEachChar
                |> List.map getRemainingUnits
                |> List.sortWith sortByLengthDesc

        -- _ =
        --     Debug.log "polymers" polymers
    in
    Maybe.withDefault "" (List.head polymers)


sortByLengthDesc : String -> String -> Order
sortByLengthDesc a b =
    compare (String.length a) (String.length b)


getUnitsWithoutChar : Char -> List Char -> String
getUnitsWithoutChar char units =
    units |> List.filter (\s -> Char.toLower s /= char) |> String.fromList


reduce : Char -> List Char -> List Char
reduce nextChar remaining =
    -- learned this nice solution here https://github.com/jwoLondon/adventOfCode/blob/master/literateElm/d05_2018.md
    case List.head remaining of
        Nothing ->
            [ nextChar ]

        Just leftChar ->
            if react leftChar nextChar then
                List.drop 1 remaining

            else
                [ nextChar ] ++ remaining


react : Char -> Char -> Bool
react left right =
    (Char.toLower left == Char.toLower right) && left /= right



{-
   The previous solution was very very very ineficient because it started at the beginning
   of the string each time it found two chars that reacted
-}
-- getRemainingUnits : String -> String
-- getRemainingUnits originalUnits =
--     originalUnits
--         |> String.toList
--         |> List.map Char.toCode
--         |> helper []
--         |> List.map Char.fromCode
--         |> String.fromList
-- helper : List Int -> List Int -> List Int
-- helper remaining tokens =
--     -- let
--     --     _ =
--     --         Debug.log "helper call" (formatToDebug tokens remaining)
--     -- in
--     case tokens of
--         first :: second :: tail ->
--             let
--                 difference =
--                     first - second
--                 _ =
--                     if difference == 32 || difference == -32 then
--                         Debug.log "got difference" ("size is " ++ String.fromInt (List.length tokens))
--                     else
--                         ""
--                 -- _ =
--                 --     Debug.log "tokens" tokens
--                 -- _ =
--                 --     Debug.log "remaining" remaining
--                 -- _ =
--                 --     Debug.log "comparing" (String.fromInt first ++ " - " ++ String.fromInt second ++ " = " ++ String.fromInt difference)
--             in
--             if difference == 32 || difference == -32 then
--                 helper [] (remaining ++ tail)
--             else
--                 helper (remaining ++ [ first ]) (second :: tail)
--         last :: [] ->
--             -- let
--             --     _ =
--             --         Debug.log "last" last
--             -- in
--             helper (remaining ++ [ last ]) []
--         [] ->
--             remaining
-- formatToDebug tokens remaining =
--     "helper" ++ formatList tokens ++ formatList remaining
-- formatList : List Int -> String
-- formatList list =
--     " [" ++ mapToChars list ++ "] "
-- mapToChars : List Int -> String
-- mapToChars list =
--     List.map Char.fromCode list |> String.fromList
