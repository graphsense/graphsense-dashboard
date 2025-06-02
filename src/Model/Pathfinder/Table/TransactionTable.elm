module Model.Pathfinder.Table.TransactionTable exposing (Model, filter, titleHash, titleTimestamp, titleValue)

import Api.Data
import Api.Request.Addresses
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction)
import Model.Graph.Table as Table
import Msg.Pathfinder.AddressDetails exposing (Msg)
import PagedTable


type alias Model =
    { table : PagedTable.Model Api.Data.AddressTx
    , order : Maybe Api.Request.Addresses.Order_
    , dateRangePicker : Maybe (DateRangePicker.Model Msg)
    , txMinBlock : Maybe Int
    , txMaxBlock : Maybe Int
    , direction : Maybe Direction
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
