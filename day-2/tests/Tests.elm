module Tests exposing (tests)

import DayInput exposing (dayInput)
import Dict exposing (..)
import Expect
import InventoryManagement exposing (findCommonLetters, findTwoCorrectBoxIDs, hasCommonLetters, hasNDuplicateLetters)
import Test exposing (..)


hasOneCommonLetter =
    hasCommonLetters 1


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
        , describe "Find the common letters between two correct box IDs"
            [ test "Checks if there are only one different char at the same position between strings \"abcde\" and \"axcye\" " <|
                \_ ->
                    Expect.equal False (hasOneCommonLetter "abcde" "axcye")
            , test "Checks if there are only one different char at the same position between strings \"abcde\" and \"fghij\" " <|
                \_ ->
                    Expect.equal False (hasOneCommonLetter "abcde" "fghij")
            , test "Checks if there are only one different char at the same position between strings \"fghij\" and \"fguij\" " <|
                \_ ->
                    Expect.equal True (hasOneCommonLetter "fghij" "fguij")
            , test "Finds substring of common letters between two strings" <|
                \_ ->
                    let
                        input =
                            ( "abcde", "abcdx" )
                    in
                    Expect.equal "abcd" (findCommonLetters input)
            , test "Finds two correct box ids from example input" <|
                \_ ->
                    let
                        input =
                            [ "abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz" ]
                    in
                    Expect.equal ( "fghij", "fguij" ) (findTwoCorrectBoxIDs input)
            , test "From example input" <|
                \_ ->
                    let
                        input =
                            [ "abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz" ]
                    in
                    input
                        |> findTwoCorrectBoxIDs
                        |> findCommonLetters
                        |> Expect.equal "fgij"
            , test "From day input" <|
                \_ ->
                    let
                        input =
                            dayInput
                    in
                    input
                        |> findTwoCorrectBoxIDs
                        |> findCommonLetters
                        |> Expect.equal "jiwamotgsfrudclzbyzkhlrvp"
            ]
        ]
