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
                            , guardId = Just 10
                            , action = BeginsShift
                            }
                    in
                    "[1518-11-01 00:00] Guard #10 begins shift"
                        |> ActionRecordParser.fromString
                        |> Expect.equal (Ok expected)
            , test "Parses '[1518-11-01 00:05] falls asleep'" <|
                \_ ->
                    let
                        expected =
                            { year = 1518
                            , month = 11
                            , day = 1
                            , hour = 0
                            , minute = 5
                            , guardId = Nothing
                            , action = FallsAsleep
                            }
                    in
                    "[1518-11-01 00:05] falls asleep"
                        |> ActionRecordParser.fromString
                        |> Expect.equal (Ok expected)
            , test "Parses '[1518-11-01 00:25] wakes up'" <|
                \_ ->
                    let
                        expected =
                            { year = 1518
                            , month = 11
                            , day = 1
                            , hour = 0
                            , minute = 25
                            , guardId = Nothing
                            , action = WakesUp
                            }
                    in
                    "[1518-11-01 00:25] wakes up"
                        |> ActionRecordParser.fromString
                        |> Expect.equal (Ok expected)
            ]
        ]
