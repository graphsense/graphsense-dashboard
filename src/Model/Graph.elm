module Model.Graph exposing (..)

import Api.Data
import Browser.Dom as Dom
import Color exposing (Color)
import Config.Graph exposing (Config)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import Model.Graph.Adding as Adding
import Model.Graph.Browser as Browser
import Model.Graph.ContextMenu as ContextMenu
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Id exposing (AddressId, EntityId, LinkId)
import Model.Graph.Layer exposing (Layer)
import Model.Graph.Search as Search
import Model.Graph.Tag as Tag
import Model.Graph.Tool as Tool
import Model.Graph.Transform as Transform
import Plugin.Model as Plugin exposing (PluginStates)
import Set exposing (Set)


type alias Model =
    { config : Config
    , layers : IntDict Layer
    , browser : Browser.Model
    , adding : Adding.Model
    , dragging : Dragging
    , transform : Transform.Model
    , size : Maybe Coords
    , selected : Selected
    , hovered : Hovered
    , contextMenu : Maybe ContextMenu.Model
    , tag : Maybe Tag.Model
    , search : Maybe Search.Model
    , userAddressTags : Dict ( String, String ) Tag.UserTag
    , hovercardTBD : Maybe Dom.Element
    , entityConcepts : List Api.Data.Concept
    , abuseConcepts : List Api.Data.Concept
    , activeTool : ActiveTool
    }


type alias ActiveTool =
    { element : Maybe ( Dom.Element, Bool ) -- visibility
    , toolbox : Tool.Toolbox
    }


type NodeType
    = Address
    | Entity


type Selected
    = SelectedAddress AddressId
    | SelectedEntity EntityId
    | SelectedAddresslink (LinkId AddressId)
    | SelectedNone


type Hovered
    = HoveredEntityLink (LinkId EntityId)
    | HoveredAddressLink (LinkId AddressId)
    | HoveredNone


type Dragging
    = NoDragging
    | Dragging Transform.Model Coords Coords
    | DraggingNode EntityId Coords Coords
