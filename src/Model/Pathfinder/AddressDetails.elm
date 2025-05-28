module Model.Pathfinder.AddressDetails exposing (Model)

import Api.Data
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import PagedTable
import RemoteData exposing (WebData)


type alias Model =
    { neighborsTableOpen : Bool
    , transactionsTableOpen : Bool
    , tokenBalancesOpen : Bool
    , txs : TransactionTable.Model
    , neighborsIncoming : PagedTable.Model Api.Data.NeighborAddress
    , neighborsOutgoing : PagedTable.Model Api.Data.NeighborAddress
    , addressId : Id
    , data : Api.Data.Address
    , relatedAddresses : WebData RelatedAddressesTable.Model
    , relatedAddressesTableOpen : Bool
    , totalReceivedDetailsOpen : Bool
    , balanceDetailsOpen : Bool
    , totalSentDetailsOpen : Bool
    , outgoingNeighborsTableOpen : Bool
    , incomingNeighborsTableOpen : Bool
    , copyIconChevronOpen : Bool
    , isClusterDetailsOpen : Bool
    , displayAllTagsInDetails : Bool
    }
