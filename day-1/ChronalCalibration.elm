module ChronalCalibration exposing
    ( calculateResultingFrequency
    , findFirstFrequencyThatRepeatsTwice
    )

import Array exposing (..)
import Set exposing (..)


calculateResultingFrequency : List Int -> Int
calculateResultingFrequency frequencies =
    List.sum frequencies


findFirstFrequencyThatRepeatsTwice : Array Int -> Int
findFirstFrequencyThatRepeatsTwice frequencies =
    firstFrequencyHelper frequencies 0 0 (Set.insert 0 Set.empty)


firstFrequencyHelper : Array Int -> Int -> Int -> Set Int -> Int
firstFrequencyHelper frequencies currentIndex currentFrequency achievedFrequencies =
    let
        nextFrequency =
            Maybe.withDefault 0 (Array.get currentIndex frequencies)

        newFrequency =
            currentFrequency + nextFrequency

        wasNewFrequencyAlreadyAchieved =
            Set.member newFrequency achievedFrequencies

        -- _ =
        --     Debug.log "Achieved Frequencies" achievedFrequencies
        -- _ =
        --     Debug.log "Is repeated" wasNewFrequencyAlreadyAchieved
        -- _ =
        --     Debug.log ""
        --         ("Current frequency "
        --             ++ String.fromInt currentFrequency
        --             ++ ", change of "
        --             ++ String.fromInt nextFrequency
        --             ++ "; resulting frequency "
        --             ++ String.fromInt newFrequency
        --         )
    in
    if wasNewFrequencyAlreadyAchieved then
        newFrequency

    else
        let
            newAchievedFrequencies =
                Set.insert newFrequency achievedFrequencies

            {-
                by using modulo we can traverse the array like a circular list
               "modBy n a" (or "a modulo n" or "a % n", in other languages) is modular arithmetic
               it works this way:
               - if n is zero it is division by zero, therefore not allowed (error or undefined depending on lang)
               - the result is always an integer
               - if n is greater than a the result is a
               for example:

                modBy 0 5 -> invalid
                modBy 2 10 -> 0
                modBy 5 2 -> 2

                a video to better understand: https://www.youtube.com/watch?v=b5cb_nfDyyM
            -}
            nextIndex =
                modBy (Array.length frequencies) (currentIndex + 1)

            -- _ =
            --     Debug.log "Partial frequencies" partialFrequencies
        in
        firstFrequencyHelper frequencies nextIndex newFrequency newAchievedFrequencies
