module Msg.Pathfinder.AddressDetails exposing (Msg(..))

import Api.Data
import Api.Request.Addresses
import DurationDatePicker
import Model.Direction exposing (Direction)
import Model.Pathfinder.Id exposing (Id)
import Time exposing (Posix)


type Msg
    = UserClickedToggleNeighborsTable
    | UserClickedToggleTransactionTable
    | UserClickedNextPageTransactionTable
    | UserClickedPreviousPageTransactionTable
    | UserClickedNextPageNeighborsTable Direction
    | UserClickedPreviousPageNeighborsTable Direction
    | GotTxsForAddressDetails Id Api.Data.AddressTxs
    | GotNextPageTxsForAddressDetails Id Api.Data.AddressTxs
    | GotNeighborsForAddressDetails Id Direction Api.Data.NeighborAddresses
    | UpdateDateRangePicker DurationDatePicker.Msg
    | OpenDateRangePicker
    | CloseDateRangePicker
    | ResetDateRangePicker
    | BrowserGotFromDateBlock Posix Api.Data.BlockAtDate
    | BrowserGotToDateBlock Posix Api.Data.BlockAtDate
