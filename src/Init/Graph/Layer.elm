module Init.Graph.Layer exposing (init)

import Config.Graph exposing (..)
import Dict
import Model.Graph.Layer exposing (..)


init : Float -> Int -> Layer
init x id =
    { id = id
    , entities = Dict.empty
    , x = x
    }
