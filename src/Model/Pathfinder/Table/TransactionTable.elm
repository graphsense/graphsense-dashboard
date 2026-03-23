module Model.Pathfinder.Table.TransactionTable exposing (Model, filter, titleHash, titleTimestamp, titleValue)

import Api.Data
import Api.Request.Addresses
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import View.Pathfinder.TransactionFilter as TransactionFilter


type alias Model =
    { table : InfiniteTable.Model Api.Data.AddressTx
    , order : Maybe Api.Request.Addresses.Order_
    , filter : TransactionFilter.Model
    , isTxFilterViewOpen : Bool
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
