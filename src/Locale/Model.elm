module Locale.Model exposing (Model, getString)

import Dict exposing (Dict)


type alias Model =
    { mapping : Dict String String
    , locale : String
    }


getString : Model -> String -> String
getString { mapping } key =
    let
        lower =
            String.toLower key
                |> Debug.log "lower"

        raise s =
            if String.left 1 key /= String.left 1 lower then
                (String.left 1 s
                    |> String.toUpper
                )
                    ++ String.dropLeft 1 s

            else
                s
    in
    Dict.get lower mapping
        |> Debug.log "found"
        |> Maybe.withDefault key
        |> raise
