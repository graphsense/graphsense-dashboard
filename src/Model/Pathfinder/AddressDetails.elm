module Model.Pathfinder.AddressDetails exposing (..)

import Api.Data
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.PagedTable exposing (PagedTable)
import Model.Pathfinder.Table.TransactionTable as TransactionTable


type alias Model =
    { neighborsTableOpen : Bool
    , transactionsTableOpen : Bool
    , txs : TransactionTable.Model
    , neighborsIncoming : PagedTable Api.Data.NeighborAddress
    , neighborsOutgoing : PagedTable Api.Data.NeighborAddress
    , address : Address
    , data : Api.Data.Address
    }
