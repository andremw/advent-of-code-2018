module Tests exposing (tests)

import DayInput exposing (dayInput)
import Expect
import Pathfinder exposing (findOrderOfSteps)
import Set exposing (Set)
import Test exposing (..)


tests : Test
tests =
    describe "Day 7: The Sum of Its Parts"
        [ describe
            "Part 1 - Find order of steps"
            [ test "From example input" <|
                \_ ->
                    let
                        input =
                            [ "Step C must be finished before step A can begin."
                            , "Step C must be finished before step F can begin."
                            , "Step A must be finished before step B can begin."
                            , "Step A must be finished before step D can begin."
                            , "Step B must be finished before step E can begin."
                            , "Step D must be finished before step E can begin."
                            , "Step F must be finished before step E can begin."
                            ]
                    in
                    Expect.equal "CABDFE" (findOrderOfSteps input)
            ]

        -- , skip <|
        --     describe "Part two - "
        --         []
        ]
