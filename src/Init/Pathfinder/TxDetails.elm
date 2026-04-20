module Init.Pathfinder.TxDetails exposing (init, initIoTable, initSubTxTable)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Components.TransactionFilter as TransactionFilter
import Effect.Pathfinder exposing (Effect(..))
import Model.Pathfinder.Table.IoTable as IoTable
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Model.Pathfinder.TxDetails as TxDetails
import RemoteData
import Util.Data exposing (negateTxValue)


initSubTxTable : InfiniteTable.Model Api.Data.TxAccount
initSubTxTable =
    Table.initUnsorted
        |> InfiniteTable.init "subTxTable" 6


initIoTable : String -> List Api.Data.TxValue -> InfiniteTable.Model Api.Data.TxValue
initIoTable tableId data =
    let
        baseTable =
            Table.initUnsorted

        ( model, _, _ ) =
            baseTable
                |> InfiniteTable.init tableId 6
                |> InfiniteTable.setData dummyIoTableConfig IoTable.filter Nothing data
    in
    model



-- Dummy config for initializing IoTable - fetch and abort won't be used during init


dummyIoTableConfig : InfiniteTable.Config Effect
dummyIoTableConfig =
    { fetch = \_ _ _ -> BatchEffect []
    , force = False
    , effectToTracker = \_ -> Nothing
    , abort = \_ -> BatchEffect []
    , triggerOffset = 100
    }


init : Maybe TransactionFilter.Settings -> List String -> Tx -> TxDetails.Model
init txsFilter assets tx =
    let
        ( inputs, outputs ) =
            case tx.type_ of
                Tx.Utxo { raw } ->
                    ( raw.inputs
                        |> Maybe.withDefault []
                        |> List.map negateTxValue
                    , raw.outputs
                        |> Maybe.withDefault []
                    )

                Tx.Account _ ->
                    ( [], [] )
    in
    { inputsTableOpen = False
    , outputsTableOpen = False
    , inputsTable = initIoTable "inputsTable" inputs
    , outputsTable = initIoTable "outputsTable" outputs
    , tx = tx
    , baseTx = RemoteData.NotAsked
    , subTxsTable = initSubTxTable
    , subTxsTableOpen =
        tx
            |> Tx.getAccountTx
            |> Maybe.map
                (\t ->
                    (t.raw.isExternal
                        |> Maybe.withDefault False
                        |> not
                    )
                        || (t.value.value == 0)
                )
            |> Maybe.withDefault False
    , subTxsTableFilter =
        txsFilter
            |> Maybe.withDefault
                (TransactionFilter.initSettings
                    |> TransactionFilter.withIncludeZeroValueTxs False
                )
            |> TransactionFilter.init
            |> TransactionFilter.withAssetSelectBox assets
    }
