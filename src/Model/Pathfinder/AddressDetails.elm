module Model.Pathfinder.AddressDetails exposing (Model)

import Api.Data
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.PagedTable exposing (PagedTable)
import Model.Pathfinder.Table.TransactionTable as TransactionTable


type alias Model =
    { neighborsTableOpen : Bool
    , transactionsTableOpen : Bool
    , tokenBalancesOpen : Bool
    , txs : TransactionTable.Model
    , neighborsIncoming : PagedTable Api.Data.NeighborAddress
    , neighborsOutgoing : PagedTable Api.Data.NeighborAddress
    , addressId : Id
    , data : Api.Data.Address
    }
