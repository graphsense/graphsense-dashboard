module Model.Graph.History.Entry exposing (..)

import Color exposing (Color)
import IntDict exposing (IntDict)
import Model.Graph.Layer exposing (Layer)


type alias Model =
    { layers : IntDict Layer
    , highlights : List ( String, Color )
    }
