module Model.Pathfinder.TxDetails exposing (Model, SubTxTableFilter)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Table exposing (Table)
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction)
import Model.Pathfinder.Tx exposing (Tx)
import Msg.Pathfinder exposing (TxDetailsMsg)
import RemoteData exposing (WebData)
import Util.ThemedSelectBox as ThemedSelectBox


type alias SubTxTableFilter =
    { includeZeroValueTxs : Maybe Bool
    , isSubTxsTableFilterDialogOpen : Bool
    , selectedAsset : Maybe String
    , dateRangePicker : Maybe (DateRangePicker.Model TxDetailsMsg)
    , direction : Maybe Direction
    , assetSelectBox : ThemedSelectBox.Model (Maybe String)
    }


type alias Model =
    { inputsTableOpen : Bool
    , outputsTableOpen : Bool
    , inputsTable : Table Api.Data.TxValue
    , outputsTable : Table Api.Data.TxValue
    , tx : Tx
    , subTxsTableOpen : Bool
    , baseTx : WebData Api.Data.TxAccount
    , subTxsTable : InfiniteTable.Model Api.Data.TxAccount
    , subTxsTableFilter : SubTxTableFilter
    }
