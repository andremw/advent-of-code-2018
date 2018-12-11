module Tests exposing (tests)

import DayInput exposing (dayInput)
import Dict exposing (..)
import Expect
import InventoryManagement exposing (hasNDuplicateLetters)
import Test exposing (..)


tests : Test
tests =
    describe "Day 2 - Inventory Management System"
        [ describe "hasNDuplicateLetters"
            [ test "Checks that \"abcdef\" doesn't have 2 duplicates" <|
                \_ ->
                    Expect.equal False (hasNDuplicateLetters 2 "abcdef")
            , test "Checks that \"abbcde\" has two duplicates" <|
                \_ ->
                    Expect.equal True (hasNDuplicateLetters 2 "abbcde")
            , test "Checks that \"abbcde\" doesn't have three duplicates" <|
                \_ ->
                    Expect.equal False (hasNDuplicateLetters 3 "abbcde")
            , test "Checks that \"bababc\" has three duplicates" <|
                \_ ->
                    Expect.equal True (hasNDuplicateLetters 3 "bababc")
            ]
        , describe "Calculate Checksum"
            [ test "From example input" <|
                \_ ->
                    let
                        input =
                            [ "abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab" ]

                        with2Duplicates =
                            List.filter (hasNDuplicateLetters 2) input

                        with3Duplicates =
                            List.filter (hasNDuplicateLetters 3) input
                    in
                    Expect.equal 12 (List.length with2Duplicates * List.length with3Duplicates)
            , test "From exercise input" <|
                \_ ->
                    let
                        input =
                            dayInput

                        with2Duplicates =
                            List.filter (hasNDuplicateLetters 2) input

                        with3Duplicates =
                            List.filter (hasNDuplicateLetters 3) input
                    in
                    Expect.equal 5904 (List.length with2Duplicates * List.length with3Duplicates)
            ]
        ]
