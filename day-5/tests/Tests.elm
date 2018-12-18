module Tests exposing (tests)

import AlchemicalReduction exposing (getRemainingUnits, getShortestPolymer)
import DayInput exposing (dayInput)
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "Day 5: Alchemical Reduction"
        [ describe "Part 1 - Calculates remaining units after reaction"
            [ test "From 'aA'" <|
                \_ ->
                    getRemainingUnits "aA" |> Expect.equal ""
            , test "From 'abBA'" <|
                \_ ->
                    getRemainingUnits "abBA" |> Expect.equal ""
            , test "From 'abAB'" <|
                \_ ->
                    getRemainingUnits "abAB" |> Expect.equal "abAB"
            , test "From 'aabAAB'" <|
                \_ ->
                    getRemainingUnits "aabAAB" |> Expect.equal "aabAAB"
            , test "From example input" <|
                \_ ->
                    let
                        input =
                            "dabAcCaCBAcCcaDA"
                    in
                    getRemainingUnits input
                        |> String.length
                        |> Expect.equal 10
            , test "From exercise input" <|
                \_ ->
                    let
                        input =
                            dayInput |> String.trim
                    in
                    getRemainingUnits input |> String.length |> Expect.equal 11364
            ]
        , describe "Part two - Get shortest polymer"
            [ test "From example input" <|
                \_ ->
                    let
                        input =
                            "dabAcCaCBAcCcaDA"
                    in
                    getShortestPolymer input |> String.length |> Expect.equal 4
            , test "From exercise input" <|
                \_ ->
                    let
                        input =
                            dayInput
                    in
                    getShortestPolymer input |> String.length |> Expect.equal 4
            ]
        ]
