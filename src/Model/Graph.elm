module Model.Graph exposing (..)

import Color exposing (Color)
import Config.Graph exposing (Config)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import Model.Graph.Adding as Adding
import Model.Graph.Browser as Browser
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Id exposing (AddressId, EntityId)
import Model.Graph.Layer exposing (Layer)
import Model.Graph.Transform as Transform
import Set exposing (Set)


type alias Model =
    { config : Config
    , layers : IntDict Layer
    , browser : Browser.Model
    , adding : Adding.Model
    , dragging : Dragging
    , transform : Transform.Model
    , size : Maybe Coords
    , selected : Maybe Selected
    }


type NodeType
    = Address
    | Entity


type Selected
    = AddressId AddressId
    | EntityId EntityId


type Dragging
    = NoDragging
    | Dragging Transform.Model Coords
    | DraggingNode EntityId Coords
