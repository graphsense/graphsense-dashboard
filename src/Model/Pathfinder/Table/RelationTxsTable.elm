module Model.Pathfinder.Table.RelationTxsTable exposing (Model, filter, titleHash, titleTimestamp, titleValue)

import Api.Data
import Api.Request.Addresses
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction)
import Msg.Pathfinder.RelationDetails exposing (Msg)
import Util.ThemedSelectBox as ThemedSelectBox


type alias Model =
    { table : InfiniteTable.Model Api.Data.Link
    , order : Maybe Api.Request.Addresses.Order_
    , dateRangePicker : Maybe (DateRangePicker.Model Msg)
    , direction : Maybe Direction
    , isTxFilterViewOpen : Bool
    , assetSelectBox : ThemedSelectBox.Model (Maybe String)
    , selectedAsset : Maybe String
    , includeZeroValueTxs : Maybe Bool -- Backend does not support this filter at the moment
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
