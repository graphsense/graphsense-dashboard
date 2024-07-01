module Init.Pathfinder.Table.TransactionTable exposing (..)

import Api.Data
import Config.DateRangePicker exposing (datePickerSettings)
import Init.DateRangePicker as DateRangePicker
import Init.Graph.Table
import Model.Locale as Locale
import Model.Pathfinder.Address as Address exposing (Address)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder.AddressDetails exposing (Msg(..))


init : Locale.Model -> Address -> Api.Data.Address -> TransactionTable.Model
init locale address data =
    let
        m =
            Init.Graph.Table.initUnsorted
    in
    { table =
        { table = m
        , nrItems = Just <| data.noIncomingTxs + data.noOutgoingTxs
        , currentPage = 1
        , itemsPerPage = 5
        }
    , dateRangePicker =
        let
            ( mn, mx ) =
                Address.getActivityRange data
        in
        datePickerSettings locale mn mx
            |> DateRangePicker.init UpdateDateRangePicker mx
    }
