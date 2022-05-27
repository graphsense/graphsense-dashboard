module Msg.Graph exposing (..)

import Api.Data
import Browser.Dom
import Json.Encode
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Id exposing (AddressId, EntityId, LinkId)
import Model.Graph.Transform as Transform
import Plugin.Model as Plugin
import Table
import Time


type Msg
    = UserClickedGraph
    | UserClickedAddress AddressId
    | UserRightClickedAddress AddressId Coords
    | UserHoversAddress AddressId
    | UserClickedEntity EntityId
    | UserRightClickedEntity EntityId
    | UserHoversEntity EntityId
    | UserHoversEntityLink (LinkId EntityId)
    | UserHoversAddressLink (LinkId AddressId)
    | UserLeavesThing
    | UserClickedEntityExpandHandle EntityId Bool
    | UserClickedAddressExpandHandle AddressId Bool
    | UserClickedAddressesExpand EntityId
    | UserPushesLeftMouseButtonOnGraph Coords
    | UserMovesMouseOnGraph Coords
    | UserReleasesMouseButton
    | BrowserGotSvgElement (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserGotBrowserElement (Result Browser.Dom.Error Browser.Dom.Element)
    | UserWheeledOnGraph Float Float Float
    | UserPushesLeftMouseButtonOnEntity EntityId Coords
    | BrowserGotEntityNeighbors EntityId Bool Api.Data.NeighborEntities
    | BrowserGotEntityEgonet String Int Bool Api.Data.NeighborEntities
    | BrowserGotEntityEgonetForAddress String String Int Bool Api.Data.NeighborEntities
    | BrowserGotAddressNeighbors AddressId Bool Api.Data.NeighborAddresses
    | BrowserGotNow Time.Posix
    | BrowserGotAddress Api.Data.Address
    | BrowserGotEntity Api.Data.Entity
    | BrowserGotEntityForAddress String Api.Data.Entity
    | BrowserGotEntityForAddressNeighbor
        { anchor : AddressId
        , isOutgoing : Bool
        , neighbors : List Api.Data.NeighborAddress
        }
        Api.Data.Entity
    | BrowserGotAddressTxs { currency : String, address : String } Api.Data.AddressTxs
    | BrowserGotEntityAddresses EntityId Api.Data.EntityAddresses
    | BrowserGotEntityAddressesForTable { currency : String, entity : Int } Api.Data.EntityAddresses
    | BrowserGotAddressTags { currency : String, address : String } Api.Data.AddressTags
    | PluginMsg String Json.Encode.Value
    | TableNewState Table.State
    | UserClickedContextMenu
    | UserLeftContextMenu
    | UserClickedAnnotateAddress AddressId
    | UserClickedRemoveAddress AddressId
    | UserClickedAddressInEntityAddressesTable EntityId Api.Data.Address
    | NoOp
