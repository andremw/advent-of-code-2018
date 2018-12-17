module ActionRecordParser exposing (Action(..), ActionRecord, fromString)

import Parser exposing (..)


type Action
    = BeginsShift
    | FallsAsleep
    | WakesUp


type alias ActionRecord =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , guardId : Int
    , action : Action
    }


fromString : String -> Result String ActionRecord
fromString =
    Parser.run parseActionRecord >> Result.mapError Parser.deadEndsToString


parseActionRecord : Parser ActionRecord
parseActionRecord =
    -- "[1518-11-01 00:00] Guard #10 begins shift"
    succeed ActionRecord
        |. symbol "["
        |= int
        |. symbol "-"
        |= int
        |. symbol "-"
        |= recordInt
        |. spaces
        |= recordInt
        |. symbol ":"
        |= recordInt
        |. symbol "]"
        |. chompUntil "#"
        |. symbol "#"
        |= int
        |. spaces
        |= guardAction


recordInt : Parser Int
recordInt =
    getChompedString (chompWhile Char.isDigit)
        |> andThen stringToDay


stringToDay : String -> Parser Int
stringToDay day =
    succeed (String.toInt day |> Maybe.withDefault 0)


guardAction : Parser Action
guardAction =
    getChompedString (chompWhile (\c -> Char.isAlpha c || c == ' '))
        |> andThen stringToAction


stringToAction : String -> Parser Action
stringToAction actionString =
    let
        _ =
            Debug.log "action string" actionString
    in
    case actionString of
        "begins shift" ->
            succeed BeginsShift

        "falls asleep" ->
            succeed FallsAsleep

        "wakes up" ->
            succeed WakesUp

        other ->
            problem ("Could not understand the guard action. Got " ++ other)
