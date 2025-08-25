module Model.Pathfinder.AddressDetails exposing (Model)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Table.RelatedAddressesPubkeyTable as RelatedAddressesPubkeyTable
import Model.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder.AddressDetails exposing (RelatedAddressTypes)
import RemoteData exposing (WebData)
import Util.ThemedSelectBox as ThemedSelectBox


type alias Model =
    { neighborsTableOpen : Bool
    , transactionsTableOpen : Bool
    , tokenBalancesOpen : Bool
    , txs : WebData TransactionTable.Model
    , neighborsIncoming : WebData (InfiniteTable.Model Api.Data.NeighborAddress)
    , neighborsOutgoing : WebData (InfiniteTable.Model Api.Data.NeighborAddress)
    , address : Address
    , relatedAddresses : WebData RelatedAddressesTable.Model
    , relatedAddressesPubkey : WebData RelatedAddressesPubkeyTable.Model
    , relatedAddressesVisibleTableSelectBox : ThemedSelectBox.Model RelatedAddressTypes
    , relatedAddressesVisibleTable : RelatedAddressTypes
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
