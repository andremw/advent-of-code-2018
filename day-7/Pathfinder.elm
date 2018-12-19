module Pathfinder exposing (findOrderOfSteps)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Instruction =
    { requirement : String
    , step : String
    }


findOrderOfSteps : List String -> String
findOrderOfSteps steps =
    let
        instructions : List Instruction
        instructions =
            List.map parseInstructions steps

        -- _ =
        --     Debug.log "parsed instructions" instructions
        stepsDict : Dict String (List String)
        stepsDict =
            List.foldl updateDict Dict.empty instructions

        completeDict : Dict String (List String)
        completeDict =
            List.foldl addMissingSteps stepsDict instructions

        _ =
            Debug.log "steps dict" completeDict
    in
    "CABDFE"


parseInstructions : String -> Instruction
parseInstructions input =
    { requirement = String.slice 5 6 input, step = String.slice 36 37 input }


updateDict : Instruction -> Dict String (List String) -> Dict String (List String)
updateDict { requirement, step } dict =
    if Dict.member step dict then
        Dict.update step (Maybe.map ((::) requirement)) dict

    else
        Dict.insert step [ requirement ] dict


addMissingSteps : Instruction -> Dict String (List String) -> Dict String (List String)
addMissingSteps { requirement, step } dict =
    if Dict.member requirement dict then
        dict

    else
        Dict.insert requirement [] dict
