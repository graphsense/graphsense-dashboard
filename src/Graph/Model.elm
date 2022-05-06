module Graph.Model exposing (..)

import Graph.Model.Adding as Adding
import Graph.Model.Layer exposing (Layer)
import Set exposing (Set)


type alias Model =
    { layers : List Layer
    , adding : Adding.Model
    }
