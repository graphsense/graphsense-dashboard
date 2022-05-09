module Model.Graph exposing (..)

import Color exposing (Color)
import Dict exposing (Dict)
import Model.Graph.Adding as Adding
import Model.Graph.Layer exposing (Layer)
import Set exposing (Set)


type alias Model =
    { layers : List Layer
    , adding : Adding.Model
    , colors : Dict String Color
    }


type NodeType
    = Address
    | Entity
