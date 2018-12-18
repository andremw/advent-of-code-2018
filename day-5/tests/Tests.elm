module Tests exposing (tests)

import AlchemicalReduction exposing (getRemainingUnits)
import DayInput exposing (dayInput)
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "Day 5: Alchemical Reduction"
        [ describe "Calculates remaining units after reaction"
            [ skip <|
                test "From 'aA'" <|
                    \_ ->
                        getRemainingUnits "aA" |> Expect.equal ""
            , skip <|
                test "From 'abBA'" <|
                    \_ ->
                        getRemainingUnits "abBA" |> Expect.equal ""
            , skip <|
                test "From 'abAB'" <|
                    \_ ->
                        getRemainingUnits "abAB" |> Expect.equal "abAB"
            , skip <|
                test "From 'aabAAB'" <|
                    \_ ->
                        getRemainingUnits "aabAAB" |> Expect.equal "aabAAB"
            , skip <|
                test "From example input" <|
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
                    getRemainingUnits input |> String.length |> Expect.equal 10
            ]
        ]
