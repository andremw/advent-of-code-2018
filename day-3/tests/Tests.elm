module Tests exposing (tests)

import ClaimParser
import DayInput exposing (dayInput)
import Expect
import SquareInches exposing (..)
import Test exposing (..)


tests : Test
tests =
    describe "Day 3 - No Matter How You Slice It"
        [ describe "ClaimParser"
            [ skip <|
                test """
                Throws an error
                """ <|
                    \_ ->
                        let
                            expected =
                                { id = 1
                                , x = 896
                                , y = 863
                                , width = 29
                                , height = 19
                                }
                        in
                        "1 @ 896,863: 29x19"
                            |> ClaimParser.fromString
                            |> Expect.equal (Err "I expected a '#' indicating the claim ID")
            , test """
                Parses #1 @ 896,863: 29x19 as a
                Claim { id: 1, x: 896, y: 863, width: 29, height: 19 }
                """ <|
                \_ ->
                    let
                        expected =
                            { id = 1
                            , x = 896
                            , y = 863
                            , width = 29
                            , height = 19
                            }
                    in
                    "#1 @ 896,863: 29x19"
                        |> ClaimParser.fromString
                        |> Expect.equal (Ok expected)
            ]
        , describe "Calculate square inches of fabric"
            [ test "Creates tuple for a single claim" <|
                \_ ->
                    let
                        claim =
                            { id = 1, x = 1, y = 3, width = 2, height = 2 }
                    in
                    claim
                        |> SquareInches.createTuplesForClaim
                        |> Expect.equal
                            [ ( 2, 4 ), ( 2, 5 ), ( 3, 4 ), ( 3, 5 ) ]
            , test "Given the example input" <|
                \_ ->
                    let
                        input =
                            [ "#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2" ]
                    in
                    input
                        |> SquareInches.calculateSquareInches
                        |> Expect.equal 4
            , skip <|
                test "From day input" <|
                    \_ ->
                        let
                            input =
                                dayInput |> String.trim |> String.lines |> List.map String.trim
                        in
                        input
                            |> SquareInches.calculateSquareInches
                            |> Expect.equal 112418
            ]
        ]
