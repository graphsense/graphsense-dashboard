module Msg.Pathfinder.AddressDetails exposing (Msg(..))

import Api.Data
import DurationDatePicker
import Model.Direction exposing (Direction)
import Table
import Time exposing (Posix)


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
    | UserClickedToggleRelatedAddressesTable
    | UserClickedPreviousPageRelatedAddressesTable
    | UserClickedNextPageRelatedAddressesTable
    | UserClickedFirstPageRelatedAddressesTable
