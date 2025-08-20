module Msg.Pathfinder.AddressDetails exposing (Msg(..), RelatedAddressTypes(..), relatedAddressTypeOptions)

import Api.Data
import DurationDatePicker
import Model.Direction exposing (Direction)
import Model.Pathfinder.Id exposing (Id)
import PagedTable
import Table
import Time
import Util.Tag as Tag
import Util.ThemedSelectBox as ThemedSelectBox


type RelatedAddressTypes
    = Pubkey
    | Clusters


relatedAddressTypeOptions : List RelatedAddressTypes
relatedAddressTypeOptions =
    [ Pubkey
    , Clusters
    ]


type Msg
    = UserClickedToggleTokenBalancesSelect
    | UserClickedToggleTransactionTable
    | UserClickedToggleNeighborsTable Direction
    | UserClickedToggleBalanceDetails
    | UserClickedToggleTotalReceivedDetails
    | UserClickedToggleTotalSpentDetails
    | UserClickedToggleClusterDetailsOpen
    | UserClickedToggleDisplayAllTagsInDetails
    | TransactionsTablePagedTableMsg PagedTable.Msg
    | NeighborsTablePagedTableMsg Direction PagedTable.Msg
    | GotTxsForAddressDetails ( Maybe Time.Posix, Maybe Time.Posix ) Api.Data.AddressTxs
    | GotNextPageTxsForAddressDetails Api.Data.AddressTxs
    | GotNeighborsForAddressDetails Direction Api.Data.NeighborAddresses
    | GotNeighborsNextPageForAddressDetails Direction Api.Data.NeighborAddresses
    | UpdateDateRangePicker DurationDatePicker.Msg
    | ToggleTxFilterView
    | CloseTxFilterView
    | OpenDateRangePicker
    | CloseDateRangePicker
    | ResetDateRangePicker
    | ResetAllTxFilters
    | ResetTxDirectionFilter
    | ResetTxAssetFilter
    | TxTableFilterShowAllTxs
    | TxTableFilterShowIncomingTxOnly
    | TxTableFilterShowOutgoingTxOnly
    | TxTableAssetSelectBoxMsg (ThemedSelectBox.Msg (Maybe String))
    | TableMsg Table.State
    | RelatedAddressesTableMsg Table.State
    | RelatedAddressesPubkeyTableMsg Table.State
    | BrowserGotEntityAddressesForRelatedAddressesTable Api.Data.EntityAddresses
    | BrowserGotEntityAddressTagsForRelatedAddressesTable String Api.Data.AddressTags
    | UserClickedToggleRelatedAddressesTable
    | RelatedAddressesTablePagedTableMsg PagedTable.Msg
    | RelatedAddressesPubkeyTablePagedTableMsg PagedTable.Msg
    | UserClickedAddressCheckboxInTable Id
    | UserClickedAggEdgeCheckboxInTable Direction Id Api.Data.NeighborAddress
    | UserClickedTxCheckboxInTable Api.Data.AddressTx
    | UserClickedAllTxCheckboxInTable
    | UserClickedTx Id
    | NoOp
    | BrowserGotAddressesForTags (Maybe String) (List Api.Data.Address)
    | BrowserGotPubkeyRelations Api.Data.RelatedAddresses
    | TooltipMsg Tag.Msg
    | RelatedAddressesVisibleTableSelectBoxMsg (ThemedSelectBox.Msg RelatedAddressTypes)
