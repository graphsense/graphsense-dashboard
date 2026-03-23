module Update.Pathfinder.TxDetails exposing (loadTxDetailsDataAccount, update)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Components.TransactionFilter as TransactionFilter
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..), effectToTracker)
import Init.Pathfinder.TxDetails exposing (initSubTxTable)
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Model.Pathfinder.TxDetails exposing (Model)
import Msg.Pathfinder exposing (IoDirection(..), Msg(..), TxDetailsMsg(..))
import RecordSetter exposing (s_baseTx, s_isSubTxsTableFilterDialogOpen, s_state, s_subTxsTable)
import RemoteData
import Tuple exposing (mapFirst, mapSecond)
import Util exposing (n)
import Util.Data as Data


transactionTableConfig : Model -> InfiniteTable.Config Effect
transactionTableConfig m =
    let
        baseTxHash =
            m.tx |> Tx.getRawBaseTxHashForTx

        currency =
            m.tx |> Tx.getNetwork
    in
    { fetch =
        \_ pagesize nextpage ->
            (BrowserGotTxFlows nextpage >> TxDetailsMsg)
                |> Api.ListTxFlowsEffect
                    { currency = currency
                    , txHash = baseTxHash
                    , includeZeroValueSubTxs =
                        TransactionFilter.getIncludeZeroValueTxs m.subTxsTableFilter
                            |> Maybe.withDefault False
                    , pagesize = Just pagesize
                    , token_currency = TransactionFilter.getSelectedAsset m.subTxsTableFilter
                    , nextpage = nextpage
                    }
                |> ApiEffect
    , force = False
    , triggerOffset = 100
    , effectToTracker = effectToTracker
    , abort = Api.CancelEffect >> ApiEffect
    }


transactionTableFilter : Table.Filter Api.Data.TxAccount
transactionTableFilter =
    { search =
        \_ _ -> True
    , filter = always True
    }


loadTxDetailsDataAccount : Tx -> Model -> ( Model, List Effect )
loadTxDetailsDataAccount tx model =
    let
        config =
            transactionTableConfig model

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
            |> mapFirst (s_baseTx RemoteData.Loading)
            |> mapSecond ((++) effects)

    else
        n model


reloadSubTxTable : Model -> ( Model, List Effect )
reloadSubTxTable m =
    n
        { m
            | baseTx = RemoteData.NotAsked
            , subTxsTable = initSubTxTable
        }


update : TxDetailsMsg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        TransactionFilterMsg subMsg ->
            let
                newFilter =
                    TransactionFilter.update subMsg model.subTxsTableFilter

                changed =
                    TransactionFilter.hasChanged model.subTxsTableFilter newFilter
            in
            { model | subTxsTableFilter = newFilter }
                |> (if changed then
                        reloadSubTxTable

                    else
                        n
                   )

        UserClickedToggleSubTxsTableFilter ->
            model
                |> s_isSubTxsTableFilterDialogOpen (not model.isSubTxsTableFilterDialogOpen)
                |> n

        BrowserGotBaseTx tx ->
            n
                { model
                    | baseTx =
                        tx
                            |> Tx.getAccountTxRaw
                            |> Maybe.map RemoteData.Success
                            |> Maybe.withDefault RemoteData.NotAsked
                }

        BrowserGotTxFlows fetchedPage txs ->
            let
                config =
                    transactionTableConfig model

                accountTxs =
                    txs.txs |> List.filterMap Tx.getAccountTxRaw

                setter =
                    if fetchedPage == Nothing then
                        InfiniteTable.setData

                    else
                        InfiniteTable.appendData

                ( nt, cmd, meff ) =
                    model.subTxsTable
                        |> setter config transactionTableFilter txs.nextPage accountTxs
            in
            ( { model | subTxsTable = nt }, CmdEffect (Cmd.map (TableMsgSubTxTable >> TxDetailsMsg) cmd) :: meff )

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
                ( pt, cmd, eff ) =
                    InfiniteTable.update (transactionTableConfig model) m model.subTxsTable
            in
            ( { model | subTxsTable = pt }, CmdEffect (Cmd.map (TableMsgSubTxTable >> TxDetailsMsg) cmd) :: eff )

        UserClickedTxInSubTxsTable _ ->
            -- handled upstream
            n model

        UserClickedToggleSubTxsTable ->
            let
                ( table, eff ) =
                    if model.subTxsTableOpen then
                        InfiniteTable.abort
                            (transactionTableConfig model)
                            model.subTxsTable

                    else
                        ( model.subTxsTable, [] )
            in
            ( { model | subTxsTableOpen = not model.subTxsTableOpen, subTxsTable = table }
            , eff
            )
