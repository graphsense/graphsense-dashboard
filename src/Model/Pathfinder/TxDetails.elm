module Model.Pathfinder.TxDetails exposing (Model)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Table exposing (Table)
import Model.Pathfinder.Tx exposing (Tx)
import RemoteData exposing (WebData)
import View.Pathfinder.TransactionFilter as TransactionFilter


type alias Model =
    { inputsTableOpen : Bool
    , outputsTableOpen : Bool
    , inputsTable : Table Api.Data.TxValue
    , outputsTable : Table Api.Data.TxValue
    , tx : Tx
    , subTxsTableOpen : Bool
    , baseTx : WebData Api.Data.TxAccount
    , subTxsTable : InfiniteTable.Model Api.Data.TxAccount
    , isSubTxsTableFilterDialogOpen : Bool
    , subTxsTableFilter : TransactionFilter.Model
    }
