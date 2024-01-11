module Model.Graph exposing (..)

import Api.Data
import Browser.Dom as Dom
import Color
import Config.Graph exposing (Config)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import Model.Address as A
import Model.Entity as E
import Model.Graph.Adding as Adding
import Model.Graph.Browser as Browser
import Model.Graph.ContextMenu as ContextMenu
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Highlighter as Highlighter
import Model.Graph.Id exposing (AddressId, EntityId, LinkId)
import Model.Graph.Layer exposing (Layer)
import Model.Graph.Search as Search
import Model.Graph.Tag as Tag
import Model.Graph.Tool as Tool
import Model.Graph.Transform as Transform
import Route.Graph


type alias Model =
    { config : Config
    , layers : IntDict Layer
    , route : Route.Graph.Route
    , browser : Browser.Model
    , adding : Adding.Model
    , dragging : Dragging
    , transform : Transform.Model
    , selected : Selected
    , hovered : Hovered
    , contextMenu : Maybe ContextMenu.Model
    , tag : Maybe Tag.Model
    , search : Maybe Search.Model
    , userAddressTags : Dict ( String, String, String ) Tag.UserTag
    , activeTool : ActiveTool
    , history : History
    , highlights : Highlighter.Model
    , selectIfLoaded : Maybe SelectIfLoaded
    }


type alias History =
    { past : List (IntDict Layer)
    , future : List (IntDict Layer)
    }


type alias ActiveTool =
    { element : Maybe ( Dom.Element, Bool ) -- visibility
    , toolbox : Tool.Toolbox
    }


type NodeType
    = AddressType
    | EntityType


type SelectIfLoaded
    = SelectAddress A.Address
    | SelectEntity E.Entity
    | SelectAddresslink (Maybe Route.Graph.AddresslinkTable) A.Address A.Address
    | SelectEntitylink (Maybe Route.Graph.AddresslinkTable) E.Entity E.Entity


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
    , highlights : List ( String, Color.Color )
    }


type alias DeserializedAddress =
    { id : AddressId
    , x : Float
    , y : Float
    , userTag : Maybe Tag.UserTag
    , color : Maybe Color.Color
    }


type DeserializedEntityTag
    = TagUserTag Tag.UserTag
    | DeserializedEntityUserTagTag DeserializedEntityUserTag


type alias DeserializedEntity =
    { id : EntityId
    , rootAddress : Maybe String
    , x : Float
    , y : Float
    , color : Maybe Color.Color
    , userTag : Maybe DeserializedEntityTag
    , noAddresses : Int
    }


type alias DeserializedEntityUserTag =
    { currency : String
    , entity : Int
    , label : String
    , source : String
    , category : Maybe String
    , abuse : Maybe String
    }
