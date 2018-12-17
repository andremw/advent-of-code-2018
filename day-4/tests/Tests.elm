module Tests exposing (tests)

import ActionRecordParser exposing (Action(..), ActionRecord)
import DayInput exposing (dayInput)
import Expect
import ReposeRecord exposing (findWhenToSneakIn, findWhenToSneakIn2)
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
        , describe "Finds best chance to sneak in"
            [ describe "Strategy 1"
                [ test "From example input" <|
                    \_ ->
                        let
                            input =
                                [ "[1518-11-01 00:00] Guard #10 begins shift"
                                , "[1518-11-01 00:05] falls asleep"
                                , "[1518-11-01 00:25] wakes up"
                                , "[1518-11-01 00:30] falls asleep"
                                , "[1518-11-01 00:55] wakes up"
                                , "[1518-11-01 23:58] Guard #99 begins shift"
                                , "[1518-11-02 00:40] falls asleep"
                                , "[1518-11-02 00:50] wakes up"
                                , "[1518-11-03 00:05] Guard #10 begins shift"
                                , "[1518-11-03 00:24] falls asleep"
                                , "[1518-11-03 00:29] wakes up"
                                , "[1518-11-04 00:02] Guard #99 begins shift"
                                , "[1518-11-04 00:36] falls asleep"
                                , "[1518-11-04 00:46] wakes up"
                                , "[1518-11-05 00:03] Guard #99 begins shift"
                                , "[1518-11-05 00:45] falls asleep"
                                , "[1518-11-05 00:55] wakes up"
                                ]
                        in
                        Expect.equal 240 (findWhenToSneakIn input)
                , test "From exercise input" <|
                    \_ ->
                        let
                            input =
                                dayInput
                        in
                        Expect.equal 84636 (findWhenToSneakIn input)
                ]
            , describe "Strategy 2"
                [ test "From example input" <|
                    \_ ->
                        let
                            input =
                                [ "[1518-11-01 00:00] Guard #10 begins shift"
                                , "[1518-11-01 00:05] falls asleep"
                                , "[1518-11-01 00:25] wakes up"
                                , "[1518-11-01 00:30] falls asleep"
                                , "[1518-11-01 00:55] wakes up"
                                , "[1518-11-01 23:58] Guard #99 begins shift"
                                , "[1518-11-02 00:40] falls asleep"
                                , "[1518-11-02 00:50] wakes up"
                                , "[1518-11-03 00:05] Guard #10 begins shift"
                                , "[1518-11-03 00:24] falls asleep"
                                , "[1518-11-03 00:29] wakes up"
                                , "[1518-11-04 00:02] Guard #99 begins shift"
                                , "[1518-11-04 00:36] falls asleep"
                                , "[1518-11-04 00:46] wakes up"
                                , "[1518-11-05 00:03] Guard #99 begins shift"
                                , "[1518-11-05 00:45] falls asleep"
                                , "[1518-11-05 00:55] wakes up"
                                ]
                        in
                        Expect.equal 4455 (findWhenToSneakIn2 input)
                , test "From exercise input" <|
                    \_ ->
                        let
                            input =
                                dayInput
                        in
                        Expect.equal 91679 (findWhenToSneakIn2 input)
                ]
            ]
        ]
