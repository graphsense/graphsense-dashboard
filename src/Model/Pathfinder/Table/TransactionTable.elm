module Model.Pathfinder.Table.TransactionTable exposing (Model, filter, resetFilters, titleHash, titleTimestamp, titleValue)

import Api.Data
import Api.Request.Addresses
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction)
import Msg.Pathfinder.AddressDetails exposing (Msg)
import Util.ThemedSelectBox as ThemedSelectBox


type alias Model =
    { table : InfiniteTable.Model Api.Data.AddressTx
    , order : Maybe Api.Request.Addresses.Order_
    , dateRangePicker : Maybe (DateRangePicker.Model Msg)
    , direction : Maybe Direction
    , isTxFilterViewOpen : Bool
    , assetSelectBox : ThemedSelectBox.Model (Maybe String)
    , selectedAsset : Maybe String
    , includeZeroValueTxs : Maybe Bool -- Backend does not support this filter at the moment
    , downloadingCSV : Bool
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


resetFilters : Model -> Model
resetFilters model =
    { model
        | order = Nothing
        , isTxFilterViewOpen = False
        , selectedAsset = Nothing
    }
