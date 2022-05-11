module Msg.Graph exposing (..)

import Api.Data
import Browser.Dom
import Model.Graph.Id exposing (AddressId, EntityId)


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
    | UserPushesLeftMouseButtonOnGraph { x : Float, y : Float }
    | UserMovesMouseOnGraph { x : Float, y : Float }
    | UserReleasesMouseButton
    | BrowserGotSvgElement (Result Browser.Dom.Error Browser.Dom.Element)
    | UserWheeledOnGraph Float Float Float
    | NoOp
