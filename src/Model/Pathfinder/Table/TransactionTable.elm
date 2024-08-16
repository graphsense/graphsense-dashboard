module Model.Pathfinder.Table.TransactionTable exposing (..)

import Api.Data
import Api.Request.Addresses
import Model.DateRangePicker as DateRangePicker
import Model.Graph.Table as Table
import Model.Pathfinder.Table exposing (PagedTable)
import Msg.Pathfinder.AddressDetails exposing (Msg)


type alias Model =
    { table : PagedTable Api.Data.AddressTx
    , order : Maybe Api.Request.Addresses.Order_
    , dateRangePicker : Maybe (DateRangePicker.Model Msg)
    , txMinBlock : Maybe Int
    , txMaxBlock : Maybe Int
    }


titleHash : String
titleHash =
    "TxHash"


titleValue : String
titleValue =
    "Value"


titleTimestamp : String
titleTimestamp =
    "Timestamp"


filter : Table.Filter Api.Data.AddressTx
filter =
    { search =
        \term a ->
            case a of
                Api.Data.AddressTxTxAccount tx ->
                    String.contains term tx.txHash

                Api.Data.AddressTxAddressTxUtxo tx ->
                    String.contains term tx.txHash
    , filter = always True
    }
