module Init.Pathfinder.Table.TransactionTable exposing (..)

import Api.Data
import Config.DateRangePicker exposing (datePickerSettings)
import Init.DateRangePicker as DateRangePicker
import Init.Graph.Table
import Model.Locale as Locale
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder.AddressDetails exposing (Msg(..))


init : Locale.Model -> Api.Data.Address -> TransactionTable.Model
init locale address =
    let
        m =
            Init.Graph.Table.initUnsorted
    in
    { table =
        { table = m
        , nrItems = Just <| address.noIncomingTxs + address.noOutgoingTxs
        , currentPage = 1
        , itemsPerPage = 5
        }
    , dateRangePicker =
        let
            ( mn, mx ) =
                Address.getActivityRange address
        in
        datePickerSettings locale mn mx
            |> DateRangePicker.init UpdateDateRangePicker mx
    }
