module Model.Graph exposing (..)

import Api.Data
import Api.Request.Entities
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
    , activeTool : ActiveTool
    , history : History
    }


type History
    = History (List (IntDict Layer)) (List (IntDict Layer))


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
    | SelectedEntitylink (LinkId EntityId)
    | SelectedNone


type Hovered
    = HoveredEntityLink (LinkId EntityId)
    | HoveredAddressLink (LinkId AddressId)
    | HoveredAddress AddressId
    | HoveredEntity EntityId
    | HoveredNone


type Dragging
    = NoDragging
    | Dragging Transform.Model Coords Coords
    | DraggingNode EntityId Coords Coords


type alias Deserializing =
    { deserialized : Deserialized
    , addresses : List Api.Data.Address
    , entities : List Api.Data.Entity
    }


type alias Deserialized =
    { addresses : List DeserializedAddress
    , entities : List DeserializedEntity
    }


type alias DeserializedAddress =
    { id : AddressId
    , x : Float
    , y : Float
    , userTag : Maybe Tag.UserTag
    }


type alias DeserializedEntity =
    { id : EntityId
    , rootAddress : Maybe String
    , x : Float
    , y : Float
    }
