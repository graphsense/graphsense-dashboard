module Model.Pathfinder.Details.AddressDetails exposing (..)

import Api.Data
import Model.Pathfinder.Table exposing (PagedTable)


type alias Model =
    { neighborsTableOpen : Bool
    , transactionsTableOpen : Bool
    , txs : PagedTable Api.Data.AddressTx
    , txMinBlock : Maybe Int
    , txMaxBlock : Maybe Int
    , neighborsIncoming : PagedTable Api.Data.NeighborAddress
    , neighborsOutgoing : PagedTable Api.Data.NeighborAddress
    }
