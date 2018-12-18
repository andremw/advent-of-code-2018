module AlchemicalReduction exposing (getRemainingUnits)


getRemainingUnits : String -> String
getRemainingUnits originalUnits =
    originalUnits
        |> String.toList
        |> List.map Char.toCode
        |> helper []
        |> List.map Char.fromCode
        |> String.fromList


helper : List Int -> List Int -> List Int
helper remaining tokens =
    -- let
    --     _ =
    --         Debug.log "helper call" (formatToDebug tokens remaining)
    -- in
    case tokens of
        first :: second :: tail ->
            let
                difference =
                    first - second

                _ =
                    if difference == 32 || difference == -32 then
                        Debug.log "got difference" ("size is " ++ String.fromInt (List.length tokens))

                    else
                        ""

                -- _ =
                --     Debug.log "tokens" tokens
                -- _ =
                --     Debug.log "remaining" remaining
                -- _ =
                --     Debug.log "comparing" (String.fromInt first ++ " - " ++ String.fromInt second ++ " = " ++ String.fromInt difference)
            in
            if difference == 32 || difference == -32 then
                helper [] (remaining ++ tail)

            else
                helper (remaining ++ [ first ]) (second :: tail)

        last :: [] ->
            -- let
            --     _ =
            --         Debug.log "last" last
            -- in
            helper (remaining ++ [ last ]) []

        [] ->
            remaining


formatToDebug tokens remaining =
    "helper" ++ formatList tokens ++ formatList remaining


formatList : List Int -> String
formatList list =
    " [" ++ mapToChars list ++ "] "


mapToChars : List Int -> String
mapToChars list =
    List.map Char.fromCode list |> String.fromList
