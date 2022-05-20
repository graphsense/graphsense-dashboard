module Msg.Graph exposing (..)

import Api.Data
import Browser.Dom
import Json.Encode
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Id exposing (AddressId, EntityId, LinkId)
import Model.Graph.Transform as Transform
import Plugin
import Time


type Msg
    = UserClickedGraph
    | UserClickedAddress AddressId
    | UserRightClickedAddress AddressId
    | UserHoversAddress AddressId
    | UserClickedEntity EntityId
    | UserRightClickedEntity EntityId
    | UserHoversEntity EntityId
    | UserHoversEntityLink (LinkId EntityId)
    | UserHoversAddressLink (LinkId AddressId)
    | UserLeavesThing
    | UserClickedEntityExpandHandle EntityId Bool
    | UserClickedAddressExpandHandle AddressId Bool
    | UserPushesLeftMouseButtonOnGraph Coords
    | UserMovesMouseOnGraph Coords
    | UserReleasesMouseButton
    | BrowserGotSvgElement (Result Browser.Dom.Error Browser.Dom.Element)
    | UserWheeledOnGraph Float Float Float
    | UserPushesLeftMouseButtonOnEntity EntityId Coords
    | BrowserGotEntityNeighbors EntityId Bool Api.Data.NeighborEntities
    | BrowserGotEntityEgonet String Int Bool Api.Data.NeighborEntities
    | BrowserGotAddressNeighbors AddressId Bool Api.Data.NeighborAddresses
    | BrowserGotNow Time.Posix
    | BrowserGotAddress Api.Data.Address
    | BrowserGotEntity String Api.Data.Entity
    | BrowserGotEntityForAddressNeighbor
        { anchor : AddressId
        , isOutgoing : Bool
        , neighbor : Api.Data.NeighborAddress
        }
        Api.Data.Entity
    | BrowserGotAddressTxs { currency : String, address : String } Api.Data.AddressTxs
    | PluginMsg String Plugin.Place Json.Encode.Value
    | NoOp
