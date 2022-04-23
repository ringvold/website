module Bool exposing (fromString)


fromString : String -> Maybe Bool
fromString str =
    case String.toLower str of
        "true" ->
            Just True

        "false" ->
            Just False

        _ ->
            Nothing
