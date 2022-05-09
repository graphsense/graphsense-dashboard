module Msg.Graph exposing (..)

import Api.Data
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
    | NoOp
