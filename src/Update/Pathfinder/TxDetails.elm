module Update.Pathfinder.TxDetails exposing (loadTxDetailsDataAccount, update)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..), effectToTracker)
import Init.Pathfinder.TxDetails exposing (initSubTxTable)
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Model.Pathfinder.TxDetails exposing (Model)
import Msg.Pathfinder exposing (IoDirection(..), Msg(..), TxDetailsMsg(..))
import RecordSetter exposing (s_baseTx, s_includeZeroValueTxs, s_isSubTxsTableFilterDialogOpen, s_state, s_subTxsTable, s_subTxsTableFilter)
import RemoteData
import Tuple exposing (mapFirst, mapSecond)
import Util exposing (and, n)
import Util.Data as Data
import Util.ThemedSelectBox as ThemedSelectBox


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
            (BrowserGotTxFlows >> TxDetailsMsg)
                |> Api.ListTxFlowsEffect
                    { currency = currency
                    , txHash = baseTxHash
                    , includeZeroValueSubTxs = m.subTxsTableFilter.includeZeroValueTxs |> Maybe.withDefault False
                    , pagesize = Just pagesize
                    , token_currency = m.subTxsTableFilter.selectedAsset
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
        UserClickedResetZeroValueSubTxsTableFilters ->
            model
                |> s_subTxsTableFilter
                    (model.subTxsTableFilter
                        |> s_includeZeroValueTxs (Just (not (model.subTxsTableFilter.includeZeroValueTxs |> Maybe.withDefault False)))
                    )
                |> n
                |> and reloadSubTxTable

        SubTxsSelectedAssetSelectBoxMsg tsbmsg ->
            let
                subTxsTableFilter =
                    model.subTxsTableFilter

                ( newSelect, outMsg ) =
                    ThemedSelectBox.update tsbmsg subTxsTableFilter.assetSelectBox

                subTxsTableFilterNew =
                    { subTxsTableFilter
                        | assetSelectBox = newSelect
                        , selectedAsset =
                            case outMsg of
                                ThemedSelectBox.Selected table ->
                                    table

                                _ ->
                                    subTxsTableFilter.selectedAsset
                    }

                oldvalue =
                    subTxsTableFilter.selectedAsset

                newValue =
                    subTxsTableFilterNew.selectedAsset
            in
            model
                |> s_subTxsTableFilter subTxsTableFilterNew
                |> n
                |> and
                    (if oldvalue /= newValue then
                        reloadSubTxTable

                     else
                        n
                    )

        UserClickedCloseSubTxTableFilterDialog ->
            model
                |> s_subTxsTableFilter (model.subTxsTableFilter |> s_isSubTxsTableFilterDialogOpen False)
                |> n

        UserClickedResetAllSubTxsTableFilters ->
            let
                subTxsTableFilter =
                    model.subTxsTableFilter
            in
            n
                { model
                    | subTxsTableFilter =
                        { subTxsTableFilter
                            | isSubTxsTableFilterDialogOpen = False
                            , includeZeroValueTxs = Just False
                            , selectedAsset = Nothing
                        }
                }
                |> and reloadSubTxTable

        UserClickedToggleSubTxsTableFilter ->
            model
                |> s_subTxsTableFilter
                    (model.subTxsTableFilter
                        |> s_isSubTxsTableFilterDialogOpen (not model.subTxsTableFilter.isSubTxsTableFilterDialogOpen)
                    )
                |> n

        UserClickedToggleIncludeZeroValueSubTxs ->
            -- setting base Tx to NotAsked and reinit the table should cause a
            -- refetch of all data in the Update Pathfinder.elm syncDetails
            model
                |> s_subTxsTableFilter
                    (model.subTxsTableFilter
                        |> s_includeZeroValueTxs
                            (case model.subTxsTableFilter.includeZeroValueTxs of
                                Just current ->
                                    Just (not current)

                                Nothing ->
                                    Just False
                            )
                    )
                |> n
                |> and reloadSubTxTable

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

                ( nt, cmd, meff ) =
                    model.subTxsTable
                        |> InfiniteTable.appendData config transactionTableFilter txs.nextPage accountTxs
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
            n { model | subTxsTableOpen = not model.subTxsTableOpen }
