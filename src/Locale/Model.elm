module Locale.Model exposing (Model, getString)

import Dict exposing (Dict)


type alias Model =
    { mapping : Dict String String
    }


getString : Model -> String -> String
getString { mapping } key =
    Dict.get key mapping
        |> Maybe.withDefault key
