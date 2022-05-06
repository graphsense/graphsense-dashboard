module Model.Graph exposing (..)

import Model.Graph.Adding as Adding
import Model.Graph.Layer exposing (Layer)
import Set exposing (Set)


type alias Model =
    { layers : List Layer
    , adding : Adding.Model
    }
