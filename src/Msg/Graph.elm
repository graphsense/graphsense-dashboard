module Msg.Graph exposing (..)

import Api.Data
import Browser.Dom
import Json.Encode
import Model.Address as A
import Model.Entity as E
import Model.Graph exposing (Dragging)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Id exposing (AddressId, EntityId, LinkId)
import Model.Graph.Transform as Transform
import Msg.Search as Search
import Plugin.Model as Plugin
import Set exposing (Set)
import Table
import Time
import Util.InfiniteScroll as InfiniteScroll


type Msg
    = UserClickedGraph Dragging
    | UserClickedAddress AddressId
    | UserRightClickedAddress AddressId Coords
    | UserHoversAddress AddressId
    | UserClickedEntity EntityId Coords
    | UserRightClickedEntity EntityId Coords
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
    | BrowserGotAddressNeighborsTable A.Address Bool Api.Data.NeighborAddresses
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
    | BrowserGotEntityNeighborsTable E.Entity Bool Api.Data.NeighborEntities
    | BrowserGotAddressTxs A.Address Api.Data.AddressTxs
    | BrowserGotEntityAddresses EntityId Api.Data.EntityAddresses
    | BrowserGotEntityAddressesForTable E.Entity Api.Data.EntityAddresses
    | BrowserGotEntityTxs E.Entity Api.Data.AddressTxs
    | BrowserGotAddressTags A.Address Api.Data.AddressTags
    | BrowserGotAddressTagsTable A.Address Api.Data.AddressTags
    | BrowserGotEntityAddressTagsTable E.Entity Api.Data.AddressTags
    | PluginMsg String Json.Encode.Value
    | TableNewState Table.State
    | UserClickedContextMenu
    | UserLeftContextMenu
    | UserClickedAnnotateAddress AddressId
    | UserClickedRemoveAddress AddressId
    | UserClickedRemoveEntity EntityId
    | UserClickedAddressInEntityAddressesTable EntityId Api.Data.Address
    | UserClickedAddressInNeighborsTable AddressId Bool Api.Data.NeighborAddress
    | UserClickedEntityInNeighborsTable EntityId Bool Api.Data.NeighborEntity
    | InternalGraphAddedAddresses (Set AddressId)
    | InternalGraphAddedEntities (Set EntityId)
    | InfiniteScrollMsg InfiniteScroll.Msg
    | TagSearchMsg Search.Msg
    | BrowserGotAddressElementForAnnotate AddressId (Result Browser.Dom.Error Browser.Dom.Element)
    | UserInputsTagSource String
    | UserInputsTagCategory String
    | UserInputsTagAbuse String
    | UserClicksCloseTagHovercard
    | UserSubmitsTagInput
    | ToBeDone String
    | BrowserGotElementForTBD (Result Browser.Dom.Error Browser.Dom.Element)
    | RuntimeHideTBD
    | NoOp
