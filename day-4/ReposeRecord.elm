module ReposeRecord exposing (findWhenToSneakIn, findWhenToSneakIn2)

import ActionRecordParser exposing (Action(..), ActionRecord, fromString)
import Dict exposing (Dict)


findWhenToSneakIn2 : List String -> Int
findWhenToSneakIn2 records =
    let
        guardsDict =
            createGuardsDictFromRecords records
    in
    guardsDict
        |> Dict.toList
        |> List.sortWith flippedSort
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.map (\( guardId, minuteMostSlept ) -> guardId * minuteMostSlept)
        |> Maybe.withDefault 0


findWhenToSneakIn : List String -> Int
findWhenToSneakIn records =
    let
        -- _ =
        --     Debug.log "parsed records" parsedRecords
        guardsDict : Dict ( Int, Int ) Int
        guardsDict =
            createGuardsDictFromRecords records

        -- _ =
        --     Debug.log "guards dict" guardsDict
        sleepRanking : List ( Int, Int )
        sleepRanking =
            guardsDict
                |> Dict.foldl
                    (\( guardId, _ ) sleepOccurrences rankings ->
                        updateSleepRanking guardId sleepOccurrences rankings
                    )
                    Dict.empty
                |> Dict.toList
                |> sortBySleepTime

        -- _ =
        --     Debug.log "sleep ranking" sleepRanking
        sleeper =
            Maybe.withDefault ( -1, -1 ) (List.head sleepRanking)

        sleeperId =
            Tuple.first sleeper

        -- _ =
        --     Debug.log "sleeper id" sleeperId
        minuteMostSlept =
            guardsDict
                |> Dict.filter (\key value -> Tuple.first key == sleeperId)
                |> Dict.toList
                |> List.sortWith sortByOccurrences
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.map Tuple.second

        -- _ =
        --     Debug.log "minute most slept" minuteMostSlept
    in
    minuteMostSlept
        |> Maybe.map ((*) sleeperId)
        |> Maybe.withDefault 0


createGuardsDictFromRecords : List String -> Dict ( Int, Int ) Int
createGuardsDictFromRecords =
    List.sort >> List.filterMap (fromString >> Result.toMaybe) >> createGuardsDict


sortBySleepTime : List ( Int, Int ) -> List ( Int, Int )
sortBySleepTime =
    List.sortWith flippedSort


sortByOccurrences : ( ( Int, Int ), Int ) -> ( ( Int, Int ), Int ) -> Order
sortByOccurrences a b =
    flippedSort ( 1, Tuple.second a ) ( 1, Tuple.second b )


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


updateSleepRanking : Int -> Int -> Dict Int Int -> Dict Int Int
updateSleepRanking guardId minute ranking =
    if Dict.member guardId ranking then
        Dict.update guardId (Maybe.map ((+) minute)) ranking

    else
        Dict.insert guardId minute ranking
