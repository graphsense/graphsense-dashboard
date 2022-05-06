module Init.Store exposing (init)

import Dict
import Model.Store exposing (..)


init : Model
init =
    { addresses = Dict.empty
    , entities = Dict.empty
    }
