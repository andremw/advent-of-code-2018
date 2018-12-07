module ChronalCalibration exposing
    ( calculateResultingFrequency
    , findFirstFrequencyThatRepeatsTwice
    )

import Set exposing (..)


calculateResultingFrequency : List Int -> Int
calculateResultingFrequency frequencies =
    List.sum frequencies


findFirstFrequencyThatRepeatsTwice : List Int -> Int
findFirstFrequencyThatRepeatsTwice frequencies =
    firstFrequencyHelper frequencies frequencies 0 (Set.insert 0 Set.empty)


firstFrequencyHelper : List Int -> List Int -> Int -> Set Int -> Int
firstFrequencyHelper originalFrequencies partialFrequencies currentFrequency achievedFrequencies =
    let
        nextFrequency =
            Maybe.withDefault 0 (List.head partialFrequencies)

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

            nextPartialFrequencies =
                List.drop 1 partialFrequencies

            hasReachedEndOfList =
                List.isEmpty nextPartialFrequencies

            -- _ =
            --     Debug.log "Partial frequencies" partialFrequencies
        in
        -- if we have reached the end of list we start again
        if hasReachedEndOfList then
            firstFrequencyHelper
                originalFrequencies
                originalFrequencies
                newFrequency
                newAchievedFrequencies

        else
            firstFrequencyHelper
                originalFrequencies
                nextPartialFrequencies
                newFrequency
                newAchievedFrequencies
