module Msg.Graph exposing (..)

import Api.Data
import Browser.Dom
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Id exposing (AddressId, EntityId)
import Model.Graph.Transform as Transform


type Msg
    = UserClickedAddress AddressId
    | UserRightClickedAddress AddressId
    | UserHoversAddress AddressId
    | UserLeavesAddress AddressId
    | UserClickedEntity EntityId
    | UserRightClickedEntity EntityId
    | UserHoversEntity EntityId
    | UserLeavesEntity EntityId
    | UserClickedEntityExpandHandle EntityId Bool
    | UserClickedAddressExpandHandle AddressId Bool
    | UserPushesLeftMouseButtonOnGraph Coords
    | UserMovesMouseOnGraph Coords
    | UserReleasesMouseButton
    | BrowserGotSvgElement (Result Browser.Dom.Error Browser.Dom.Element)
    | UserWheeledOnGraph Float Float Float
    | UserPushesLeftMouseButtonOnEntity EntityId Coords
    | BrowserGotEntityNeighbors EntityId Bool Api.Data.NeighborEntities
    | BrowserGotEntityEgonet Int Bool Api.Data.NeighborEntities
    | NoOp
