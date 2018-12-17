module ReposeRecord exposing (findWhenToSneakIn)

import ActionRecordParser exposing (Action(..), ActionRecord, fromString)
import Dict exposing (Dict)


findWhenToSneakIn : List String -> Int
findWhenToSneakIn records =
    let
        parsedRecords =
            records |> List.sort |> List.filterMap (fromString >> Result.toMaybe)

        -- _ =
        --     Debug.log "parsed records" parsedRecords
        guardsDict =
            createGuardsDict parsedRecords

        _ =
            Debug.log "guards dict" guardsDict

        sleepRecords =
            guardsDict
                -- |> Dict.toList
                -- |> List.foldl
                --     (\data recs ->
                --         let
                --             key =
                --                 Tuple.first data
                --             ( guardId, minute ) =
                --                 key
                --             sleepOccurrences =
                --                 Tuple.second
                --         in
                --         updateOccurrences guardId recs
                --     )
                --     Dict.empty
                |> Dict.foldl
                    (\( guardId, minute ) sleepOccurrences recs ->
                        updateOccurrences guardId recs
                    )
                    Dict.empty
                |> Dict.toList
                |> sortBySleepTime

        _ =
            Debug.log "sleep records" sleepRecords

        sleeper =
            Maybe.withDefault ( -1, -1 ) (List.head sleepRecords)

        sleeperId =
            Tuple.first sleeper

        _ =
            Debug.log "sleeper id" sleeperId

        minuteMostSlept =
            guardsDict
                |> Dict.filter (\key value -> Tuple.first key == sleeperId)
                |> Dict.toList
                |> List.sortWith sortByOccurrences
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.map Tuple.second

        _ =
            Debug.log "minute most slept" minuteMostSlept
    in
    minuteMostSlept
        |> Maybe.map ((*) sleeperId)
        |> Maybe.withDefault 0


sortBySleepTime : List ( Int, Int ) -> List ( Int, Int )
sortBySleepTime =
    List.sortWith flippedSort


sortByOccurrences : ( ( Int, Int ), Int ) -> ( ( Int, Int ), Int ) -> Order
sortByOccurrences a b =
    flippedSort ( 1, Tuple.second a ) ( 1, Tuple.second b )


flippedSort : ( Int, Int ) -> ( Int, Int ) -> Order
flippedSort a b =
    case compare (Tuple.second a) (Tuple.second b) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


createGuardsDict : List ActionRecord -> Dict ( Int, Int ) Int
createGuardsDict records =
    helper Dict.empty -1 -1 records


helper : Dict ( Int, Int ) Int -> Int -> Int -> List ActionRecord -> Dict ( Int, Int ) Int
helper guardsDict guardId sleepTime records =
    case records of
        head :: tail ->
            case head.action of
                BeginsShift ->
                    helper guardsDict (Maybe.withDefault 0 head.guardId) -1 tail

                FallsAsleep ->
                    helper guardsDict guardId head.minute tail

                WakesUp ->
                    let
                        sleepRange =
                            List.range sleepTime (head.minute - 1)

                        updatedDict =
                            sleepRange
                                |> List.foldl (\sleepMinute -> updateOccurrences ( guardId, sleepMinute )) guardsDict
                    in
                    helper updatedDict guardId sleepTime tail

        [] ->
            guardsDict


updateOccurrences : comparable -> Dict comparable Int -> Dict comparable Int
updateOccurrences comp someDict =
    if Dict.member comp someDict then
        Dict.update comp (Maybe.map ((+) 1)) someDict

    else
        Dict.insert comp 1 someDict
