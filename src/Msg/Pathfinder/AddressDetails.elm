module Msg.Pathfinder.AddressDetails exposing (Msg(..))

import Api.Data
import DurationDatePicker
import Model.Direction exposing (Direction)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Table.RelatedAddressesTable exposing (ListType)
import Table
import Time exposing (Posix)
import Util.ThemedSelectBox as ThemedSelectBox


type Msg
    = UserClickedToggleNeighborsTable
    | UserClickedToggleTokenBalancesSelect
    | UserClickedToggleTransactionTable
    | UserClickedNextPageTransactionTable
    | UserClickedPreviousPageTransactionTable
    | UserClickedFirstPageTransactionTable
    | UserClickedNextPageNeighborsTable Direction
    | UserClickedPreviousPageNeighborsTable Direction
    | GotTxsForAddressDetails ( Maybe Int, Maybe Int ) Api.Data.AddressTxs
    | GotNextPageTxsForAddressDetails Api.Data.AddressTxs
    | GotNeighborsForAddressDetails Direction Api.Data.NeighborAddresses
    | UpdateDateRangePicker DurationDatePicker.Msg
    | OpenDateRangePicker
    | CloseDateRangePicker
    | ResetDateRangePicker
    | BrowserGotFromDateBlock Posix Api.Data.BlockAtDate
    | BrowserGotToDateBlock Posix Api.Data.BlockAtDate
    | TableMsg Table.State
    | RelatedAddressesTableMsg Table.State
    | BrowserGotEntityAddressesForRelatedAddressesTable Api.Data.EntityAddresses
    | BrowserGotEntityAddressTagsForRelatedAddressesTable String Api.Data.AddressTags
    | UserClickedToggleRelatedAddressesTable
    | UserClickedPreviousPageRelatedAddressesTable
    | UserClickedNextPageRelatedAddressesTable
    | UserClickedFirstPageRelatedAddressesTable
    | UserClickedAddressCheckboxInTable Id
    | SelectBoxMsg (ThemedSelectBox.Msg ListType)
    | UserClickedTxCheckboxInTable Api.Data.AddressTx
    | UserClickedTx Id
    | NoOp
    | BrowserGotAddressesForTags (Maybe String) (List Api.Data.Address)
