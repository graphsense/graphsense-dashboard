module Model.Pathfinder.Table.RelationTxsTable exposing (Model, filter, titleHash, titleTimestamp, titleValue)

import Api.Data
import Api.Request.Addresses
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Components.TransactionFilter as TransactionFilter


type alias Model =
    { table : InfiniteTable.Model Api.Data.Link
    , order : Maybe Api.Request.Addresses.Order_
    , isTxFilterViewOpen : Bool
    , filter : TransactionFilter.Model
    , exportCSV : ExportCSV.Model
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


filter : Table.Filter Api.Data.Link
filter =
    { search =
        \term a ->
            case a of
                Api.Data.LinkLinkUtxo tx ->
                    String.contains term tx.txHash

                Api.Data.LinkTxAccount tx ->
                    String.contains term tx.txHash
    , filter = always True
    }
