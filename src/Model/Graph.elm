module Model.Graph exposing (..)

import Color exposing (Color)
import Dict exposing (Dict)
import Model.Graph.Adding as Adding
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Id exposing (EntityId)
import Model.Graph.Layer exposing (Layer)
import Model.Graph.Transform as Transform
import Set exposing (Set)


type alias Model =
    { layers : List Layer
    , adding : Adding.Model
    , colors : Dict String Color
    , dragging : Dragging
    , mouse : Coords
    , transform : Transform.Model
    , width : Float
    , height : Float
    }


type NodeType
    = Address
    | Entity


type Dragging
    = NoDragging
    | Dragging Transform.Model Coords
    | DraggingNode EntityId Coords
