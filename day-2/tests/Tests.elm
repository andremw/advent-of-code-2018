module Tests exposing (tests)

import DayInput exposing (dayInput)
import Expect
import InventoryManagement exposing (..)
import Test exposing (..)


tests : Test
tests =
    describe "Day 2 - Inventory Management System"
        [ describe "Calculates the checksum for lists of box IDs"
            [ test "Example list" <|
                \_ ->
                    let
                        list =
                            [ "abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab" ]
                    in
                    Expect.equal 12 (calculateChecksum list)
            ]
        ]
