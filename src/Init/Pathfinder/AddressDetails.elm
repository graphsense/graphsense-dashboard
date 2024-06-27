module Init.Pathfinder.AddressDetails exposing (..)

import Api.Data
import DurationDatePicker exposing (TimePickerVisibility(..))
import Init.DateRangePicker as DateRangePicker
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.DateRangePicker as DateRangePicker
import Model.Locale as Locale
import Model.Pathfinder exposing (Details(..), Selection(..))
import Model.Pathfinder.Address as Address
import Model.Pathfinder.AddressDetails as AddressDetails
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import Time exposing (Posix)
import Time.Extra as Time exposing (Interval(..))
import View.Locale as Locale


init : Locale.Model -> Api.Data.Address -> AddressDetails.Model
init locale address =
    { neighborsTableOpen = False
    , transactionsTableOpen = False
    , txs = TransactionTable.init <| Just <| address.noIncomingTxs + address.noOutgoingTxs
    , txMinBlock = Nothing
    , txMaxBlock = Nothing
    , neighborsOutgoing = NeighborsTable.init address.outDegree
    , neighborsIncoming = NeighborsTable.init address.inDegree
    , dateRangePicker =
        let
            ( mn, mx ) =
                Address.getActivityRange address
        in
        datePickerSettings locale mn mx
            |> DateRangePicker.init UpdateDateRangePicker mx
    , address = address
    }


datePickerSettings : Locale.Model -> Posix -> Posix -> DurationDatePicker.Settings
datePickerSettings localeModel min max =
    let
        defaults =
            DurationDatePicker.defaultSettings localeModel.zone

        isDateBefore x datetime =
            Time.posixToMillis x > Time.posixToMillis datetime

        toDate z x =
            Time.floor Day z x
    in
    { defaults
        | isDayDisabled =
            \clientZone datetime ->
                isDateBefore (toDate clientZone datetime) (toDate clientZone max)
                    || isDateBefore (toDate clientZone min) (toDate clientZone datetime)
        , focusedDate = Just max
        , dateStringFn = \_ pos -> (pos |> Time.posixToMillis) |> (\x -> x // 1000) |> Locale.timestampDateUniform localeModel
        , timePickerVisibility = NeverVisible
        , showCalendarWeekNumbers = True
    }
