module Init.Graph.Layer exposing (init)

import Dict
import Model.Graph.Layer exposing (..)


init : Float -> Int -> Layer
init x id =
    { id = id
    , entities = Dict.empty
    , x = x
    }
