module Model.Pathfinder.TxDetails exposing (Model, hasSubTxsTable)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.TransactionFilter as TransactionFilter
import Model.Pathfinder.Tx as Tx exposing (Tx)
import RemoteData exposing (WebData)


type alias Model =
    { inputsTableOpen : Bool
    , outputsTableOpen : Bool
    , inputsTable : InfiniteTable.Model Api.Data.TxValue
    , outputsTable : InfiniteTable.Model Api.Data.TxValue
    , tx : Tx
    , subTxsTableOpen : Bool
    , baseTx : WebData Api.Data.TxAccount
    , subTxsTable : InfiniteTable.Model Api.Data.TxAccount
    , subTxsTableFilter : TransactionFilter.Model
    }


hasSubTxsTable : Tx -> Bool
hasSubTxsTable tx =
    tx
        |> Tx.getAccountTx
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False
