module Tests exposing (tests)

import Expect
import Test exposing (..)

import ChronalCalibration exposing (calculateResultingFrequency)

tests : Test
tests =
  describe "Day 1 - Chronal Calibration"
    [ describe "Calculates the resulting frequency given a list of frequencies"
        [ test "Given a simple list" <|
            \_ ->
                let
                  list = [1, 1, 1]
                in
                  Expect.equal 3 (calculateResultingFrequency list)
        ]
        ,
          test "Given another simple list" <|
              \_ ->
                  let
                    list = [1, 1, -2]
                  in
                    Expect.equal 0 (calculateResultingFrequency list)
        ,
          test "Given yet another simple list" <|
              \_ ->
                  let
                    list = [-1, -2, -3]
                  in
                    Expect.equal -6 (calculateResultingFrequency list)
    ]