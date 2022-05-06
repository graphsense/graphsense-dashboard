module Store.Init exposing (init)

import Dict
import Store.Model exposing (..)


init : Model
init =
    { addresses = Dict.empty
    , entities = Dict.empty
    }
