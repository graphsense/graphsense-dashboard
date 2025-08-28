module Update.Pathfinder.TxDetails exposing (loadTxDetailsDataAccount, update)

import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.TxDetails exposing (initSubTxTable)
import Maybe.Extra
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Model.Pathfinder.TxDetails exposing (Model, transactionTableConfig, transactionTableFilter)
import Msg.Pathfinder exposing (IoDirection(..), Msg(..), TxDetailsMsg(..))
import RecordSetter exposing (s_baseTx, s_state, s_subTxsTable)
import RemoteData
import Tuple exposing (mapFirst, mapSecond)
import Util exposing (n)
import Util.Data as Data


loadTxDetailsDataAccount : Tx -> InfiniteTable.Config Effect -> Model -> ( Model, List Effect )
loadTxDetailsDataAccount tx config model =
    let
        hasToFetchMoreData =
            Data.isAccountLike (tx.id |> Id.network) && model.baseTx == RemoteData.NotAsked

        baseTxHash =
            tx |> Tx.getRawBaseTxHashForTx

        effects =
            if hasToFetchMoreData then
                [ (BrowserGotBaseTx >> TxDetailsMsg)
                    |> Api.GetTxEffect
                        { currency = tx |> Tx.getNetwork
                        , txHash = baseTxHash
                        , tokenTxId = Nothing
                        , includeIo = False
                        }
                    |> ApiEffect
                ]

            else
                []
    in
    if hasToFetchMoreData then
        InfiniteTable.loadFirstPage config model.subTxsTable
            |> mapFirst (flip s_subTxsTable model)
            |> mapSecond Maybe.Extra.toList
            |> mapFirst (s_baseTx RemoteData.Loading)
            |> mapSecond ((++) effects)

    else
        n model


update : TxDetailsMsg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        UserClickedToggleIncludeZeroValueSubTxs ->
            -- setting base Tx to NotAsked and reinit the table should cause a
            -- refetch of all data in the Update Pathfinder.elm syncDetails
            n
                { model
                    | includeZeroValueSubTxs = not model.includeZeroValueSubTxs
                    , baseTx = RemoteData.NotAsked
                    , subTxsTable = initSubTxTable
                }

        NoOpSubTxsTable ->
            n model

        BrowserGotBaseTx tx ->
            n
                { model
                    | baseTx =
                        tx
                            |> Tx.getAccountTxRaw
                            |> Maybe.map RemoteData.Success
                            |> Maybe.withDefault RemoteData.NotAsked
                }

        BrowserGotTxFlows txs ->
            let
                config =
                    transactionTableConfig model

                accountTxs =
                    txs.txs |> List.filterMap Tx.getAccountTxRaw

                ( nt, meff ) =
                    model.subTxsTable |> InfiniteTable.appendData config transactionTableFilter txs.nextPage accountTxs
            in
            ( { model | subTxsTable = nt }, Maybe.Extra.toList meff )

        UserClickedToggleIoTable Inputs ->
            n { model | inputsTableOpen = not model.inputsTableOpen }

        UserClickedToggleIoTable Outputs ->
            n { model | outputsTableOpen = not model.outputsTableOpen }

        TableMsg Inputs state ->
            n { model | inputsTable = model.inputsTable |> s_state state }

        TableMsg Outputs state ->
            n { model | outputsTable = model.outputsTable |> s_state state }

        TableMsgSubTxTable m ->
            let
                ( pt, eff ) =
                    InfiniteTable.update (transactionTableConfig model) m model.subTxsTable
            in
            ( { model | subTxsTable = pt }, Maybe.Extra.toList eff )

        UserClickedTxInSubTxsTable _ ->
            -- handled upstream
            n model

        UserClickedToggleSubTxsTable ->
            n { model | subTxsTableOpen = not model.subTxsTableOpen }
