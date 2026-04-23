module Init.Pathfinder.TxDetails exposing (dummyIoTableConfig, init, initIoTable, initSubTxTable)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Components.TransactionFilter as TransactionFilter
import Effect.Pathfinder exposing (Effect(..))
import IntDict
import Model.Pathfinder.Table.IoTable as IoTable exposing (titleValue)
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Model.Pathfinder.TxDetails as TxDetails
import Msg.Pathfinder.TxDetails exposing (IoDirection(..))
import RemoteData
import Tuple exposing (pair)
import Tuple3
import Util.Data exposing (negateTxValue)


initSubTxTable : InfiniteTable.Model Api.Data.TxAccount
initSubTxTable =
    InfiniteTable.init "subTxTable" 6


initIoTable : String -> IoDirection -> List Api.Data.TxValue -> InfiniteTable.Model Api.Data.TxValue
initIoTable tableId ioDirection data =
    let
        dataAsc =
            data
                |> List.sortBy (.value >> .value)
    in
    InfiniteTable.init tableId 6
        |> InfiniteTable.sortBy titleValue True
        |> InfiniteTable.setData dummyIoTableConfig IoTable.filter Nothing dataAsc
        |> Tuple3.first
        |> InfiniteTable.sortBy titleValue False
        |> InfiniteTable.setData dummyIoTableConfig IoTable.filter Nothing (List.reverse dataAsc)
        |> Tuple3.first
        |> InfiniteTable.sortBy titleValue
            (case ioDirection of
                Inputs ->
                    True

                Outputs ->
                    False
            )



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
    , inputsTable = initIoTable "inputsTable" Inputs inputs
    , outputsTable = initIoTable "outputsTable" Outputs outputs
    , inputsRefs =
        inputs
            |> List.filterMap (.index >> Maybe.map (flip pair RemoteData.Loading))
            |> IntDict.fromList
    , outputsRefs =
        outputs
            |> List.filterMap (.index >> Maybe.map (flip pair RemoteData.Loading))
            |> IntDict.fromList
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
