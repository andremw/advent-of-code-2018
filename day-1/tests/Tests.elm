module Tests exposing (tests)

import Array exposing (..)
import ChronalCalibration exposing (..)
import DayInput exposing (dayInput)
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "Day 1 - Chronal Calibration"
        [ describe "Calculates the resulting frequency given a list of frequencies"
            [ test "Given a simple list" <|
                \_ ->
                    let
                        list =
                            [ 1, 1, 1 ]
                    in
                    Expect.equal 3 (calculateResultingFrequency list)
            , test "Given another simple list" <|
                \_ ->
                    let
                        list =
                            [ 1, 1, -2 ]
                    in
                    Expect.equal 0 (calculateResultingFrequency list)
            , test "Given yet another simple list" <|
                \_ ->
                    let
                        list =
                            [ -1, -2, -3 ]
                    in
                    Expect.equal -6 (calculateResultingFrequency list)
            , test "Given the day input" <|
                \_ ->
                    let
                        list =
                            dayInput
                    in
                    Expect.equal 416 (calculateResultingFrequency list)
            ]
        , describe "Finds the first frequency that repeats twice"
            [ test "Example list" <|
                \_ ->
                    let
                        list =
                            [ 1, -2, 3, 1 ]
                    in
                    Expect.equal 2 (findFirstFrequencyThatRepeatsTwice list)
            , test "First list" <|
                \_ ->
                    let
                        list =
                            [ 1, -1 ]
                    in
                    Expect.equal 0 (findFirstFrequencyThatRepeatsTwice list)
            , test "Second list" <|
                \_ ->
                    let
                        list =
                            [ 3, 3, 4, -2, -4 ]
                    in
                    Expect.equal 10 (findFirstFrequencyThatRepeatsTwice list)
            , test "Third list" <|
                \_ ->
                    let
                        list =
                            [ -6, 3, 8, 5, -6 ]
                    in
                    Expect.equal 5 (findFirstFrequencyThatRepeatsTwice list)
            , test "Fourth list" <|
                \_ ->
                    let
                        list =
                            [ 7, 7, -2, -7, -4 ]
                    in
                    Expect.equal 14 (findFirstFrequencyThatRepeatsTwice list)
            , test "Day Input" <|
                \_ ->
                    let
                        list =
                            dayInput
                    in
                    Expect.equal 56752 (findFirstFrequencyThatRepeatsTwice list)
            ]
        ]
