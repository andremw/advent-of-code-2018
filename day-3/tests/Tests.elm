module Tests exposing (tests)

import ClaimParser
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "Day 3 - No Matter How You Slice It"
        [ describe "ClaimParser"
            [ test """
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
        ]
