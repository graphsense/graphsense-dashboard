module Update.Pathfinder.TxDetails exposing (loadTxDetailsDataAccount, update)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Components.TransactionFilter as TransactionFilter
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..), effectToTracker)
import Init.Pathfinder.TxDetails exposing (dummyIoTableConfig)
import Model.Pathfinder.Id as Id exposing (TxsFilterId(..))
import Model.Pathfinder.Tx as Tx
import Model.Pathfinder.TxDetails exposing (Model, hasSubTxsTable)
import Msg.Pathfinder as Pathfinder
import Msg.Pathfinder.TxDetails exposing (IoDirection(..), Msg(..))
import RecordSetter exposing (s_baseTx, s_state, s_subTxsTable)
import RemoteData
import Tuple exposing (mapFirst, mapSecond, pair)
import Util exposing (and, n)
import Util.Data as Data


transactionTableConfig : Model -> InfiniteTable.Config Effect
transactionTableConfig m =
    let
        baseTxHash =
            m.tx |> Tx.getRawBaseTxHashForTx

        currency =
            m.tx |> Tx.getNetwork

        settings =
            TransactionFilter.getSettings m.subTxsTableFilter
    in
    { fetch =
        \_ pagesize nextpage ->
            (BrowserGotTxFlows nextpage >> Pathfinder.TxDetailsMsg)
                |> Api.ListTxFlowsEffect
                    { currency = currency
                    , txHash = baseTxHash
                    , includeZeroValueSubTxs =
                        settings
                            |> TransactionFilter.getIncludeZeroValueTxs
                            |> Maybe.withDefault False
                    , pagesize = Just pagesize
                    , token_currency = TransactionFilter.getSelectedAsset settings
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


loadFirstPage : Model -> ( Model, List Effect )
loadFirstPage model =
    let
        config =
            transactionTableConfig model
    in
    InfiniteTable.loadFirstPage config model.subTxsTable
        |> mapFirst (flip s_subTxsTable model)


loadTxDetailsDataAccount : Model -> ( Model, List Effect )
loadTxDetailsDataAccount model =
    let
        hasToFetchMoreData =
            Data.isAccountLike (model.tx.id |> Id.network) && model.baseTx == RemoteData.NotAsked

        baseTxHash =
            model.tx |> Tx.getRawBaseTxHashForTx

        effects =
            if hasToFetchMoreData then
                [ (BrowserGotBaseTx >> Pathfinder.TxDetailsMsg)
                    |> Api.GetTxEffect
                        { currency = model.tx |> Tx.getNetwork
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
        loadFirstPage model
            |> mapFirst (s_baseTx RemoteData.Loading)
            |> mapSecond ((++) effects)

    else
        n model


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        NoOp ->
            n model

        TransactionFilterMsg subMsg ->
            let
                ( newFilter, _ ) =
                    TransactionFilter.update subMsg model.subTxsTableFilter

                changed =
                    TransactionFilter.hasChanged model.subTxsTableFilter newFilter
            in
            { model | subTxsTableFilter = newFilter }
                |> (if changed then
                        flip pair
                            [ TransactionFilter.getSettings newFilter
                                |> Pathfinder.InternalChangedTxFilter (TxsFilterTx model.tx.id)
                                |> InternalEffect
                            ]
                            >> and loadFirstPage

                    else
                        n
                   )

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
            ( { model
                | subTxsTable = nt
              }
            , CmdEffect (Cmd.map (TableMsgSubTxTable >> Pathfinder.TxDetailsMsg) cmd) :: meff
            )

        UserClickedToggleIoTable Inputs ->
            n { model | inputsTableOpen = not model.inputsTableOpen }

        UserClickedToggleIoTable Outputs ->
            n { model | outputsTableOpen = not model.outputsTableOpen }

        UserClickedIoTableAddress id ->
            ( model, [ InternalEffect (Pathfinder.UserClickedAddress id) ] )

        UserClickedIoTableCheckbox id ->
            ( model, [ InternalEffect (Pathfinder.UserClickedAddressCheckboxInTable id) ] )

        UserClickedAllIoTableCheckboxes direction ->
            ( model, [ InternalEffect (Pathfinder.UserClickedAllAddressCheckboxInTable direction) ] )

        UserClickedIoTableExpand id direction ->
            let
                txId =
                    Tx.getTxIdForTx model.tx
            in
            ( model, [ InternalEffect (Pathfinder.UserClickedAddressExpandHandleInIoTable txId id direction) ] )

        TooltipMsg tooltipMsgAsTooltipType ->
            ( model, [ InternalEffect (Pathfinder.TooltipMsg tooltipMsgAsTooltipType) ] )

        TableMsg Inputs state ->
            ( { model | inputsTable = InfiniteTable.updateTable (s_state state) model.inputsTable }, [] )

        TableMsg Outputs state ->
            ( { model | outputsTable = InfiniteTable.updateTable (s_state state) model.outputsTable }, [] )

        IoTableMsg ioDir m ->
            let
                table =
                    case ioDir of
                        Inputs ->
                            model.inputsTable

                        Outputs ->
                            model.outputsTable

                ( pt, cmd, eff ) =
                    InfiniteTable.update dummyIoTableConfig m table

                ( newTable, msgTag ) =
                    if ioDir == Inputs then
                        ( { model | inputsTable = pt }, IoTableMsg Inputs )

                    else
                        ( { model | outputsTable = pt }, IoTableMsg Outputs )
            in
            ( newTable
            , CmdEffect (Cmd.map (msgTag >> Pathfinder.TxDetailsMsg) cmd) :: eff
            )

        TableMsgSubTxTable m ->
            let
                ( pt, cmd, eff ) =
                    InfiniteTable.update (transactionTableConfig model) m model.subTxsTable
            in
            ( { model | subTxsTable = pt }, CmdEffect (Cmd.map (TableMsgSubTxTable >> Pathfinder.TxDetailsMsg) cmd) :: eff )

        UserClickedTxInSubTxsTable _ ->
            -- handled upstream
            n model

        UserClickedToggleSubTxsTable ->
            if hasSubTxsTable model.tx then
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

            else
                n model
