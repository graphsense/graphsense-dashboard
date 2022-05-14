module Init.Graph.Layer exposing (init)

import Config.Graph exposing (..)
import Dict
import Model.Graph.Layer exposing (..)


init : Int -> Layer
init id =
    { id = id
    , entities = Dict.empty
    , x = toFloat id * (entityWidth + layerMargin)
    }
