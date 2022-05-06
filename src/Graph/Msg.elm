module Graph.Msg exposing (..)

import Api.Data
import Graph.Model.Id exposing (AddressId)


type Msg
    = UserClickedAddress AddressId
    | UserRightClickedAddress AddressId
    | UserHoversAddress AddressId
    | UserLeavesAddress AddressId
    | NoOp
