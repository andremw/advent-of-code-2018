module Tests exposing (tests)

-- import ReposeRecord exposing (..)

import ActionRecordParser exposing (Action(..), ActionRecord)
import DayInput exposing (dayInput)
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "Day 4: Repose Record"
        [ describe "ActionRecordParser"
            [ test "Parses '[1518-11-01 00:00] Guard #10 begins shift'" <|
                \_ ->
                    let
                        expected =
                            { year = 1518
                            , month = 11
                            , day = 1
                            , hour = 0
                            , minute = 0
                            , guardId = 10
                            , action = BeginsShift
                            }
                    in
                    "[1518-11-01 00:00] Guard #10 begins shift"
                        |> ActionRecordParser.fromString
                        |> Expect.equal (Ok expected)
            ]
        ]
