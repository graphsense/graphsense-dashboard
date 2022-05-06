module Msg.Graph exposing (..)

import Api.Data
import Model.Graph.Id exposing (AddressId)


type Msg
    = UserClickedAddress AddressId
    | UserRightClickedAddress AddressId
    | UserHoversAddress AddressId
    | UserLeavesAddress AddressId
    | NoOp
