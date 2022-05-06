module Init.Graph.Layer exposing (init)

import Config.Graph exposing (..)
import Model.Graph.Layer exposing (..)


init : Int -> Layer
init id =
    { id = id
    , entities = []
    , x = toFloat id * (entityWidth + layerMargin)
    }
