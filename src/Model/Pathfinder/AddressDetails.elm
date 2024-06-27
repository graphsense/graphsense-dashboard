module Model.Pathfinder.AddressDetails exposing (..)

import Api.Data
import Model.DateRangePicker as DateRangePicker
import Model.Pathfinder.Table exposing (PagedTable)
import Msg.Pathfinder.AddressDetails exposing (Msg)


type alias Model =
    { neighborsTableOpen : Bool
    , transactionsTableOpen : Bool
    , txs : PagedTable Api.Data.AddressTx
    , txMinBlock : Maybe Int
    , txMaxBlock : Maybe Int
    , neighborsIncoming : PagedTable Api.Data.NeighborAddress
    , neighborsOutgoing : PagedTable Api.Data.NeighborAddress
    , dateRangePicker : DateRangePicker.Model Msg
    , address : Api.Data.Address
    }
