module Msg.Pathfinder.AddressDetails exposing (Msg(..))

import Api.Data
import DurationDatePicker
import Model.Direction exposing (Direction)
import Model.Pathfinder.Id exposing (Id)
import PagedTable
import Table
import Time exposing (Posix)
import Util.Tag as Tag
import Util.ThemedSelectBox as ThemedSelectBox


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
    | GotTxsForAddressDetails ( Maybe Int, Maybe Int ) Api.Data.AddressTxs
    | GotNextPageTxsForAddressDetails Api.Data.AddressTxs
    | GotNeighborsForAddressDetails Direction Api.Data.NeighborAddresses
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
    | BrowserGotFromDateBlock Posix Api.Data.BlockAtDate
    | BrowserGotToDateBlock Posix Api.Data.BlockAtDate
    | TableMsg Table.State
    | RelatedAddressesTableMsg Table.State
    | BrowserGotEntityAddressesForRelatedAddressesTable Api.Data.EntityAddresses
    | BrowserGotEntityAddressTagsForRelatedAddressesTable String Api.Data.AddressTags
    | UserClickedToggleRelatedAddressesTable
    | RelatedAddressesTablePagedTableMsg PagedTable.Msg
    | UserClickedAddressCheckboxInTable Id
    | UserClickedTxCheckboxInTable Api.Data.AddressTx
    | UserClickedAllTxCheckboxInTable
    | UserClickedTx Id
    | NoOp
    | BrowserGotAddressesForTags (Maybe String) (List Api.Data.Address)
    | TooltipMsg Tag.Msg
