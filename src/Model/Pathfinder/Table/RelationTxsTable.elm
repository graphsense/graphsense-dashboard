module Model.Pathfinder.Table.RelationTxsTable exposing (Model, filter, titleHash, titleTimestamp, titleValue)

import Api.Data
import Api.Request.Addresses
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction)
import Model.Graph.Table as Table
import Msg.Pathfinder.RelationDetails exposing (Msg)
import PagedTable
import Util.ThemedSelectBox as ThemedSelectBox


type alias Model =
    { table : PagedTable.Model Api.Data.Link
    , order : Maybe Api.Request.Addresses.Order_
    , dateRangePicker : Maybe (DateRangePicker.Model Msg)
    , txMinBlock : Maybe Int
    , txMaxBlock : Maybe Int
    , direction : Maybe Direction
    , isTxFilterViewOpen : Bool
    , assetSelectBox : ThemedSelectBox.Model (Maybe String)
    , selectedAsset : Maybe String
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
