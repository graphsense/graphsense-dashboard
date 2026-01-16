module Update.Pathfinder.AddressDetails exposing (loadFirstTxsPage, makeExportCSVConfig, prepareCSV, syncByAddress, update)

import Api.Data
import Api.Request.Addresses
import Basics.Extra exposing (flip)
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Components.PagedTable as PagedTable
import Config.DateRangePicker exposing (datePickerSettings)
import Config.Pathfinder exposing (numberOfRowsForCSVExport)
import Config.Update as Update
import Dict exposing (Dict)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..), effectToTracker)
import Init.DateRangePicker as DateRangePicker
import Init.Pathfinder.AddressDetails exposing (getExposedAssetsForAddress)
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.DateFilter exposing (DateFilterRaw)
import Model.Direction exposing (Direction(..))
import Model.Locale as Locale
import Model.Pathfinder.Address as Address exposing (Address)
import Model.Pathfinder.AddressDetails exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.Table.NeighborsTable as NeighborsTable
import Model.Pathfinder.Table.RelatedAddressesPubkeyTable as RelatedAddressesPubkeyTable
import Model.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder as Pathfinder
import Msg.Pathfinder.AddressDetails exposing (Msg(..), RelatedAddressTypes(..))
import RecordSetter exposing (..)
import RemoteData exposing (WebData)
import Set
import Table
import Time
import Tuple exposing (first, mapFirst, mapSecond, second)
import Tuple3
import Update.DateRangePicker as DateRangePicker
import Update.Pathfinder.Table.RelatedAddressesPubkeyTable as RelatedAddressesPubkeyTable
import Update.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Util exposing (and, n)
import Util.Csv
import Util.Data as Data
import Util.ThemedSelectBox as ThemedSelectBox
import View.Locale as Locale


neighborsTableConfigWithMsg : (Direction -> Maybe String -> Api.Data.NeighborAddresses -> Msg) -> Id -> Direction -> InfiniteTable.Config Effect
neighborsTableConfigWithMsg msg addressId dir =
    { fetch =
        \_ pagesize nextpage ->
            msg dir nextpage
                >> Pathfinder.AddressDetailsMsg addressId
                |> Api.GetAddressNeighborsEffect
                    { currency = Id.network addressId
                    , address = Id.id addressId
                    , includeLabels = True
                    , onlyIds = Nothing
                    , isOutgoing = dir == Outgoing
                    , pagesize = pagesize
                    , nextpage = nextpage
                    , includeActors = False
                    }
                |> ApiEffect
    , force = False
    , triggerOffset = 100
    , effectToTracker = effectToTracker
    , abort = Api.CancelEffect >> ApiEffect
    }


neighborsTableConfig : Id -> Direction -> InfiniteTable.Config Effect
neighborsTableConfig =
    neighborsTableConfigWithMsg GotNeighborsForAddressDetails


transactionTableConfig : TransactionTable.Model Msg -> Id -> InfiniteTable.Config Effect
transactionTableConfig =
    transactionTableConfigWithMsg GotTxsForAddressDetails


transactionTableConfigWithMsg : (Maybe String -> Api.Data.AddressTxs -> Msg) -> TransactionTable.Model Msg -> Id -> InfiniteTable.Config Effect
transactionTableConfigWithMsg msg txs addressId =
    { fetch = fetchTransactions msg txs addressId
    , force = False
    , triggerOffset = 100
    , effectToTracker = effectToTracker
    , abort = Api.CancelEffect >> ApiEffect
    }


fetchTransactions : (Maybe String -> Api.Data.AddressTxs -> Msg) -> TransactionTable.Model Msg -> Id -> Maybe ( String, Bool ) -> Int -> Maybe String -> Effect
fetchTransactions msg txs addressId sorting pagesize nextpage =
    fetchTransactionsWithPathfinderMsg
        (msg nextpage
            >> Pathfinder.AddressDetailsMsg addressId
        )
        txs
        addressId
        sorting
        pagesize
        nextpage


fetchTransactionsWithPathfinderMsg : (Api.Data.AddressTxs -> Pathfinder.Msg) -> TransactionTable.Model Msg -> Id -> Maybe ( String, Bool ) -> Int -> Maybe String -> Effect
fetchTransactionsWithPathfinderMsg msg txs addressId sorting pagesize nextpage =
    Api.GetAddressTxsByDateEffect
        { currency = Id.network addressId
        , address = Id.id addressId
        , direction = txs.direction
        , pagesize = pagesize
        , nextpage = nextpage
        , order =
            sorting
                |> Maybe.andThen
                    (\( column, isReversed ) ->
                        if column == TransactionTable.titleTimestamp then
                            if isReversed then
                                Just Api.Request.Addresses.Order_Desc

                            else
                                Just Api.Request.Addresses.Order_Asc

                        else
                            txs.order
                    )
        , tokenCurrency = txs.selectedAsset
        , minDate = txs.dateRangePicker |> Maybe.andThen .fromDate
        , maxDate = txs.dateRangePicker |> Maybe.andThen .toDate
        }
        msg
        |> ApiEffect


update : Update.Config -> Msg -> Model -> ( Model, List Effect )
update uc msg model =
    case msg of
        UserClickedToggleBalanceDetails ->
            ( model |> s_balanceDetailsOpen (not model.balanceDetailsOpen), [] )

        UserClickedToggleTotalReceivedDetails ->
            ( model |> s_totalReceivedDetailsOpen (not model.totalReceivedDetailsOpen), [] )

        UserClickedToggleTotalSpentDetails ->
            ( model |> s_totalSentDetailsOpen (not model.totalSentDetailsOpen), [] )

        UserClickedToggleTokenBalancesSelect ->
            ( model |> s_tokenBalancesOpen (not model.tokenBalancesOpen), [] )

        UserClickedToggleNeighborsTable dir ->
            getNeighborsTableAndSetter model dir
                |> Maybe.map
                    (\{ table, setTable, tableOpen, setTableOpen } ->
                        let
                            conf =
                                neighborsTableConfigWithMsg GotNeighborsForAddressDetails model.address.id dir

                            ( tblNew, eff1 ) =
                                if tableOpen then
                                    InfiniteTable.abort conf table

                                else
                                    InfiniteTable.gotoFirstPage conf table
                        in
                        ( model
                            |> setTableOpen (not tableOpen)
                            |> setTable tblNew
                        , eff1
                        )
                    )
                |> Maybe.withDefault (n model)

        NeighborsTableSubTableMsg dir pm ->
            getNeighborsTableAndSetter model dir
                |> Maybe.map
                    (\{ table, setTable } ->
                        let
                            ( pt, cmd, eff ) =
                                InfiniteTable.update (neighborsTableConfig model.address.id dir) pm table
                        in
                        ( setTable pt model
                        , CmdEffect (Cmd.map (NeighborsTableSubTableMsg dir >> Pathfinder.AddressDetailsMsg model.address.id) cmd)
                            :: eff
                        )
                    )
                |> Maybe.withDefault (n model)

        GotNeighborsForAddressDetails dir fetchedPage neighbors ->
            getNeighborsTableAndSetter model dir
                |> Maybe.map
                    (\{ table, setTable } ->
                        let
                            setter =
                                if fetchedPage == Nothing then
                                    InfiniteTable.setData

                                else
                                    InfiniteTable.appendData

                            ( pt, cmd, eff ) =
                                table
                                    |> setter
                                        (neighborsTableConfig model.address.id dir)
                                        NeighborsTable.filter
                                        neighbors.nextPage
                                        neighbors.neighbors
                        in
                        ( setTable pt model
                        , CmdEffect (Cmd.map (NeighborsTableSubTableMsg dir >> Pathfinder.AddressDetailsMsg model.address.id) cmd)
                            :: eff
                        )
                    )
                |> Maybe.withDefault (n model)

        TransactionsTableSubTableMsg pm ->
            model.txs
                |> RemoteData.map
                    (\txs ->
                        let
                            ( table, cmd, eff ) =
                                InfiniteTable.update (transactionTableConfig txs model.address.id) pm txs.table
                        in
                        ( table, eff )
                            |> mapFirst (flip s_table txs)
                            |> mapFirst (RemoteData.Success >> flip s_txs model)
                            |> mapSecond
                                ((::)
                                    (cmd
                                        |> Cmd.map
                                            (TransactionsTableSubTableMsg
                                                >> Pathfinder.AddressDetailsMsg model.address.id
                                            )
                                        |> CmdEffect
                                    )
                                )
                    )
                |> RemoteData.withDefault (n model)

        UserClickedToggleTransactionTable ->
            if model.transactionsTableOpen then
                closeTransactionTable model

            else
                openTransactionTable uc Nothing model

        GotTxsForAddressDetails fetchedPage txs ->
            model.txs
                |> RemoteData.map
                    (\txsTable ->
                        let
                            setter =
                                if fetchedPage == Nothing then
                                    InfiniteTable.setData

                                else
                                    InfiniteTable.appendData

                            ( table, cmd, eff ) =
                                txsTable.table
                                    |> setter
                                        (transactionTableConfig txsTable model.address.id)
                                        TransactionTable.filter
                                        txs.nextPage
                                        txs.addressTxs
                        in
                        ( table, eff )
                            |> mapFirst (flip s_table txsTable)
                            |> mapFirst (RemoteData.Success >> flip s_txs model)
                            |> mapSecond
                                ((::)
                                    (cmd
                                        |> Cmd.map
                                            (TransactionsTableSubTableMsg
                                                >> Pathfinder.AddressDetailsMsg model.address.id
                                            )
                                        |> CmdEffect
                                    )
                                )
                    )
                |> RemoteData.withDefault (n model)

        UpdateDateRangePicker subMsg ->
            model.txs
                |> RemoteData.map
                    (\txsTable ->
                        txsTable.dateRangePicker
                            |> Maybe.map
                                (\dateRangePicker ->
                                    let
                                        newPicker =
                                            DateRangePicker.update subMsg dateRangePicker

                                        changed =
                                            newPicker.fromDate /= Nothing && newPicker.fromDate /= dateRangePicker.fromDate

                                        np =
                                            if changed then
                                                newPicker |> DateRangePicker.closePicker

                                            else
                                                newPicker
                                    in
                                    { model
                                        | txs = s_dateRangePicker (Just np) txsTable |> RemoteData.Success
                                    }
                                        |> (if changed then
                                                loadFirstTxsPage True

                                            else
                                                n
                                           )
                                )
                            |> Maybe.withDefault (n model)
                    )
                |> RemoteData.withDefault (n model)

        ToggleTxFilterView ->
            model.txs
                |> RemoteData.map
                    (\txsTable ->
                        txsTable.dateRangePicker
                            |> flip s_dateRangePicker txsTable
                            |> s_isTxFilterViewOpen (not txsTable.isTxFilterViewOpen)
                            |> RemoteData.Success
                            |> flip s_txs model
                            |> n
                    )
                |> RemoteData.withDefault (n model)

        OpenDateRangePicker ->
            model.txs
                |> RemoteData.map2
                    (\data txsTable ->
                        let
                            ( mn, mx ) =
                                Address.getActivityRange data
                        in
                        txsTable.dateRangePicker
                            |> Maybe.withDefault
                                (datePickerSettings uc.locale mn mx
                                    |> DateRangePicker.init UpdateDateRangePicker mx Nothing Nothing
                                )
                            |> DateRangePicker.openPicker
                            |> Just
                            |> flip s_dateRangePicker txsTable
                            |> RemoteData.Success
                            |> flip s_txs model
                            |> n
                    )
                    model.address.data
                |> RemoteData.withDefault (n model)

        CloseDateRangePicker ->
            model.txs
                |> RemoteData.map
                    (\txsTable ->
                        txsTable.dateRangePicker
                            |> Maybe.map DateRangePicker.closePicker
                            |> flip s_dateRangePicker txsTable
                            -- |> s_isTxFilterViewOpen False
                            |> RemoteData.Success
                            |> flip s_txs model
                            |> n
                    )
                |> RemoteData.withDefault (n model)

        CloseTxFilterView ->
            model.txs
                |> RemoteData.map (s_isTxFilterViewOpen False >> RemoteData.Success >> flip s_txs model)
                |> RemoteData.withDefault model
                |> n

        TxTableFilterShowAllTxs ->
            updateDirectionFilter model Nothing

        TxTableFilterShowIncomingTxOnly ->
            updateDirectionFilter model (Just Incoming)

        TxTableFilterShowOutgoingTxOnly ->
            updateDirectionFilter model (Just Outgoing)

        ResetAllTxFilters ->
            model.txs
                |> RemoteData.map
                    (TransactionTable.resetFilters
                        >> RemoteData.Success
                        >> flip s_txs model
                        >> loadFirstTxsPage True
                    )
                |> RemoteData.withDefault (n model)

        ResetDateRangePicker ->
            model.txs
                |> RemoteData.map
                    (s_dateRangePicker Nothing
                        >> RemoteData.Success
                        >> flip s_txs model
                        >> loadFirstTxsPage True
                    )
                |> RemoteData.withDefault (n model)

        ResetTxDirectionFilter ->
            model.txs
                |> RemoteData.map
                    (s_direction Nothing
                        >> RemoteData.Success
                        >> flip s_txs model
                        >> loadFirstTxsPage True
                    )
                |> RemoteData.withDefault (n model)

        ResetTxAssetFilter ->
            model.txs
                |> RemoteData.map
                    (s_selectedAsset Nothing
                        >> RemoteData.Success
                        >> flip s_txs model
                        >> loadFirstTxsPage True
                    )
                |> RemoteData.withDefault (n model)

        TableMsg _ ->
            n model

        RelatedAddressesTableMsg _ ->
            n model

        RelatedAddressesPubkeyTableMsg _ ->
            n model

        BrowserGotPubkeyRelations x ->
            RelatedAddressesPubkeyTable.appendAddresses x.nextPage x.relatedAddresses
                |> updateRelatedAddressesPubkeyTable model

        UserClickedToggleRelatedAddressesTable ->
            let
                show =
                    not model.relatedAddressesTableOpen

                nm =
                    { model | relatedAddressesTableOpen = show }
            in
            model.relatedAddresses
                |> RemoteData.map
                    (\ra ->
                        let
                            conf =
                                RelatedAddressesTable.tableConfig ra
                        in
                        nm
                            |> flip updateRelatedAddressesTable
                                (if show then
                                    RelatedAddressesTable.gotoFirstPage conf

                                 else
                                    RelatedAddressesTable.abort conf
                                )
                    )
                |> RemoteData.withDefault (n nm)

        RelatedAddressesTableSubTableMsg pm ->
            (\rm ->
                RelatedAddressesTable.updateTable
                    (RelatedAddressesTableSubTableMsg
                        >> Pathfinder.AddressDetailsMsg model.address.id
                    )
                    (InfiniteTable.update (RelatedAddressesTable.tableConfig rm) pm)
                    rm
            )
                |> updateRelatedAddressesTable model

        RelatedAddressesPubkeyTablePagedTableMsg pm ->
            (\rm ->
                RelatedAddressesPubkeyTable.updateTable
                    (PagedTable.update (RelatedAddressesPubkeyTable.tableConfig rm) pm)
                    rm
            )
                |> updateRelatedAddressesPubkeyTable model

        BrowserGotEntityAddressesForRelatedAddressesTable { nextPage, addresses } ->
            RelatedAddressesTable.appendEntityAddresses
                (RelatedAddressesTableSubTableMsg
                    >> Pathfinder.AddressDetailsMsg model.address.id
                )
                nextPage
                addresses
                |> updateRelatedAddressesTable model

        UserClickedTx _ ->
            n model

        UserClickedAllTxCheckboxInTable ->
            n model

        UserClickedTxCheckboxInTable _ ->
            n model

        UserClickedAddressCheckboxInTable _ ->
            n model

        UserClickedAggEdgeCheckboxInTable _ _ _ ->
            n model

        NoOp ->
            n model

        RelatedAddressesVisibleTableSelectBoxMsg ms ->
            let
                ( newSelect, outMsg ) =
                    ThemedSelectBox.update ms model.relatedAddressesVisibleTableSelectBox
            in
            n
                { model
                    | relatedAddressesVisibleTableSelectBox = newSelect
                    , relatedAddressesVisibleTable =
                        case outMsg of
                            ThemedSelectBox.Selected table ->
                                Just table

                            _ ->
                                model.relatedAddressesVisibleTable
                }

        BrowserGotEntityAddressTagsForRelatedAddressesTable currency tags ->
            let
                existingAddresses =
                    model.relatedAddresses
                        |> RemoteData.toMaybe
                        |> Maybe.map .existingTaggedAddresses
                        |> Maybe.withDefault Set.empty

                addressesToLoad =
                    List.map .address tags.addressTags
                        |> Set.fromList
                        |> flip Set.diff existingAddresses
                        |> Set.toList
            in
            if not <| List.isEmpty addressesToLoad then
                ( model
                , BrowserGotAddressesForTags tags.nextPage
                    >> Pathfinder.AddressDetailsMsg model.address.id
                    |> Api.BulkGetAddressEffect
                        { currency = currency
                        , addresses = addressesToLoad
                        }
                    |> ApiEffect
                    |> List.singleton
                )

            else
                RelatedAddressesTable.appendTaggedAddresses
                    (RelatedAddressesTableSubTableMsg
                        >> Pathfinder.AddressDetailsMsg model.address.id
                    )
                    tags.nextPage
                    []
                    |> updateRelatedAddressesTable model

        BrowserGotAddressesForTags nextpage addresses ->
            RelatedAddressesTable.appendTaggedAddresses
                (RelatedAddressesTableSubTableMsg
                    >> Pathfinder.AddressDetailsMsg model.address.id
                )
                nextpage
                addresses
                |> updateRelatedAddressesTable model

        TooltipMsg _ ->
            n model

        UserClickedToggleClusterDetailsOpen ->
            not model.isClusterDetailsOpen
                |> flip s_isClusterDetailsOpen model
                |> n

        UserClickedToggleDisplayAllTagsInDetails ->
            not model.displayAllTagsInDetails
                |> flip s_displayAllTagsInDetails model
                |> n

        TxTableAssetSelectBoxMsg ms ->
            model.txs
                |> RemoteData.map
                    (\txs ->
                        let
                            oldTxs =
                                txs

                            ( newSelect, outMsg ) =
                                ThemedSelectBox.update ms oldTxs.assetSelectBox

                            newTxs =
                                oldTxs
                                    |> s_assetSelectBox newSelect
                                    |> s_selectedAsset
                                        (case outMsg of
                                            ThemedSelectBox.Selected sel ->
                                                sel

                                            _ ->
                                                oldTxs.selectedAsset
                                        )
                        in
                        { model | txs = RemoteData.Success newTxs }
                            |> (if oldTxs.selectedAsset /= newTxs.selectedAsset then
                                    loadFirstTxsPage True

                                else
                                    n
                               )
                    )
                |> RemoteData.withDefault (n model)

        ExportCSVMsg _ _ ->
            -- handled upstream
            n model

        GotAddressTxsForExport _ _ ->
            -- handled upstream
            n model

        BrowserGotBulkTxsForExport _ _ _ _ _ _ ->
            -- handled upstream
            n model

        BrowserGotBulkTagsForExport _ _ _ _ ->
            -- handled upstream
            n model


closeTransactionTable : Model -> ( Model, List Effect )
closeTransactionTable model =
    let
        ( txs, eff ) =
            model.txs
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\txs_ ->
                        txs_.table
                            |> InfiniteTable.abort
                                (transactionTableConfig txs_ model.address.id)
                            |> mapFirst (flip s_table txs_)
                            |> mapFirst RemoteData.Success
                    )
                |> Maybe.withDefault ( model.txs, [] )
    in
    ( { model | transactionsTableOpen = False, txs = txs }
    , eff
    )


makeExportCSVConfig : Update.Config -> Id -> TransactionTable.Model Msg -> ExportCSV.Config ( Api.Data.TxAccount, Maybe Api.Data.TagSummary, Maybe Api.Data.TagSummary ) Effect
makeExportCSVConfig uc addressId txs =
    ExportCSV.config
        { filename =
            Locale.interpolated uc.locale
                "Address-transactions-of"
                [ Id.id addressId
                , Id.network addressId |> String.toUpper
                ]
        , toCsv =
            let
                nw =
                    addressId |> Id.network
            in
            prepareCSV uc.locale nw
                >> List.map (mapFirst (Locale.string uc.locale))
                >> Just
        , numberOfRows = numberOfRowsForCSVExport
        , fetch =
            \nor ->
                fetchTransactions
                    (\_ -> GotAddressTxsForExport txs)
                    txs
                    addressId
                    (txs.table
                        |> InfiniteTable.getTable
                        |> .state
                        |> Table.getSortState
                        |> Just
                    )
                    nor
                    Nothing
        , cmdToEff =
            Cmd.map
                (ExportCSVMsg txs >> Pathfinder.AddressDetailsMsg addressId)
                >> CmdEffect
        , notificationToEff = ShowNotificationEffect
        }


updateRelatedAddressesTable : Model -> (RelatedAddressesTable.Model -> ( RelatedAddressesTable.Model, List Effect )) -> ( Model, List Effect )
updateRelatedAddressesTable model upd =
    model.relatedAddresses
        |> RemoteData.map (upd >> mapFirst (RemoteData.Success >> flip s_relatedAddresses model))
        |> RemoteData.withDefault (n model)


updateRelatedAddressesPubkeyTable : Model -> (RelatedAddressesPubkeyTable.Model -> ( RelatedAddressesPubkeyTable.Model, List Effect )) -> ( Model, List Effect )
updateRelatedAddressesPubkeyTable model upd =
    model.relatedAddressesPubkey
        |> RemoteData.toMaybe
        |> Maybe.map (upd >> mapFirst (RemoteData.Success >> flip s_relatedAddressesPubkey model))
        |> Maybe.withDefault (n model)


loadFirstTxsPage : Bool -> Model -> ( Model, List Effect )
loadFirstTxsPage reset model =
    model.txs
        |> RemoteData.map
            (\nt ->
                let
                    config =
                        transactionTableConfigWithMsg
                            GotTxsForAddressDetails
                            nt
                            model.address.id

                    setter =
                        if reset then
                            InfiniteTable.loadFirstPage

                        else
                            InfiniteTable.gotoFirstPage

                    ( tableNew, eff ) =
                        nt.table
                            |> setter
                                config
                in
                ( { model
                    | txs =
                        RemoteData.Success { nt | table = tableNew }
                  }
                , eff
                )
            )
        |> RemoteData.withDefault (n model)


updateDirectionFilter : Model -> Maybe Direction -> ( Model, List Effect )
updateDirectionFilter model dir =
    model.txs
        |> RemoteData.map (s_direction dir >> RemoteData.Success >> flip s_txs model >> loadFirstTxsPage True)
        |> RemoteData.withDefault (n model)


getNeighborsTableAndSetter :
    Model
    -> Direction
    ->
        Maybe
            { table : InfiniteTable.Model Api.Data.NeighborAddress
            , setTable : InfiniteTable.Model Api.Data.NeighborAddress -> Model -> Model
            , tableOpen : Bool
            , setTableOpen : Bool -> Model -> Model
            }
getNeighborsTableAndSetter model dir =
    case dir of
        Incoming ->
            model.neighborsIncoming
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\ni ->
                        { table = ni
                        , setTable = RemoteData.Success >> s_neighborsIncoming
                        , tableOpen = model.incomingNeighborsTableOpen
                        , setTableOpen = s_incomingNeighborsTableOpen
                        }
                    )

        Outgoing ->
            model.neighborsOutgoing
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\no ->
                        { table = no
                        , setTable = RemoteData.Success >> s_neighborsOutgoing
                        , tableOpen = model.outgoingNeighborsTableOpen
                        , setTableOpen = s_outgoingNeighborsTableOpen
                        }
                    )


syncByAddress : Update.Config -> Network -> Dict Id (WebData Api.Data.Entity) -> Maybe DateFilterRaw -> Model -> Address -> ( Model, List Effect )
syncByAddress uc network clusters dateFilterPreset model address =
    address.data
        |> RemoteData.map
            (\data ->
                let
                    assets =
                        getExposedAssetsForAddress uc address

                    txs =
                        model.txs
                            |> RemoteData.map (\_ -> model.txs)
                            |> RemoteData.withDefault
                                (TransactionTable.init uc
                                    network
                                    address.id
                                    data
                                    assets
                                    UpdateDateRangePicker
                                    |> RemoteData.Success
                                )

                    cluster =
                        Id.initClusterId data.currency data.entity
                            |> flip Dict.get clusters

                    related =
                        model.relatedAddresses
                            |> RemoteData.map (\_ -> model.relatedAddresses)
                            |> RemoteData.withDefault
                                (cluster
                                    |> Maybe.map (RemoteData.map (RelatedAddressesTable.init address.id))
                                    |> Maybe.withDefault RemoteData.NotAsked
                                )

                    relatedPubkey =
                        model.relatedAddressesPubkey
                            |> RemoteData.map (\_ -> ( model.relatedAddressesPubkey, [] ))
                            |> RemoteData.withDefault
                                ( RelatedAddressesPubkeyTable.init address.id
                                    |> RemoteData.Success
                                , [ RelatedAddressesPubkeyTable.loadFirstPage address.id ]
                                )

                    relatedAddressesVisibleTable =
                        model.relatedAddressesVisibleTable
                            |> Maybe.map (\_ -> model.relatedAddressesVisibleTable)
                            |> Maybe.withDefault
                                (Maybe.andThen
                                    (\pu ->
                                        let
                                            cl =
                                                Maybe.andThen RemoteData.toMaybe cluster
                                        in
                                        if Data.isAccountLike data.currency then
                                            Just Pubkey

                                        else if cl == Nothing then
                                            Nothing

                                        else if Maybe.map (.noAddresses >> (<) 1) cl == Just True then
                                            Just MultiInputCluster

                                        else if pu > 0 then
                                            Just Pubkey

                                        else
                                            Just MultiInputCluster
                                    )
                                    (first relatedPubkey
                                        |> RemoteData.toMaybe
                                        |> Maybe.andThen (.table >> PagedTable.getNrItems)
                                    )
                                )

                    neighborsOutgoing =
                        model.neighborsOutgoing
                            |> RemoteData.map (\_ -> model.neighborsOutgoing)
                            |> RemoteData.withDefault (RemoteData.map (.outDegree >> NeighborsTable.init True) address.data)

                    neighborsIncoming =
                        model.neighborsIncoming
                            |> RemoteData.map (\_ -> model.neighborsIncoming)
                            |> RemoteData.withDefault (RemoteData.map (.outDegree >> NeighborsTable.init False) address.data)

                    newModel =
                        { model
                            | txs = txs
                            , address = address
                            , neighborsOutgoing = neighborsOutgoing
                            , neighborsIncoming = neighborsIncoming
                            , relatedAddresses = related
                            , relatedAddressesPubkey = first relatedPubkey
                            , relatedAddressesVisibleTable = relatedAddressesVisibleTable
                        }
                in
                ( newModel
                , second relatedPubkey
                )
                    |> and
                        (dateFilterPreset
                            |> Maybe.map (Just >> openTransactionTable uc)
                            |> Maybe.withDefault n
                        )
            )
        |> RemoteData.withDefault (n model)


openTransactionTable : Update.Config -> Maybe DateFilterRaw -> Model -> ( Model, List Effect )
openTransactionTable uc dfp model =
    if model.transactionsTableOpen then
        n model

    else
        model.txs
            |> RemoteData.map2
                (\data txs ->
                    let
                        ( drp, table, order ) =
                            dfp
                                |> Maybe.map
                                    (\dfp_ ->
                                        ( datePickerSettings uc.locale (dfp_.fromDate |> Maybe.withDefault mmin) (dfp_.toDate |> Maybe.withDefault mmax)
                                            |> DateRangePicker.init UpdateDateRangePicker mmax dfp_.fromDate dfp_.toDate
                                            |> Just
                                        , InfiniteTable.sortBy TransactionTable.titleTimestamp True txs.table
                                        , Just Api.Request.Addresses.Order_Desc
                                        )
                                    )
                                |> Maybe.withDefault
                                    ( txs.dateRangePicker, txs.table, txs.order )

                        ( mmin, mmax ) =
                            Address.getActivityRange data
                    in
                    { model
                        | transactionsTableOpen = True
                        , txs =
                            RemoteData.Success
                                { txs
                                    | dateRangePicker = drp
                                    , table = table
                                    , order = order
                                }
                    }
                        |> loadFirstTxsPage False
                )
                model.address.data
            |> RemoteData.withDefault (n model)


prepareCSV : Locale.Model -> String -> ( Api.Data.TxAccount, Maybe Api.Data.TagSummary, Maybe Api.Data.TagSummary ) -> List ( String, String )
prepareCSV locModel network data =
    let
        row =
            Tuple3.first data

        tagSender =
            Tuple3.second data

        tagReceiver =
            Tuple3.third data
    in
    ( "Tx_hash"
    , Util.Csv.string row.txHash
    )
        :: (if Data.isAccountLike network then
                [ ( "Token_tx_id"
                  , row.tokenTxId
                        |> Maybe.map Util.Csv.int
                        |> Maybe.withDefault (Util.Csv.string "")
                  )
                ]

            else
                []
           )
        ++ Util.Csv.valuesWithBaseCurrencyFloat "Value"
            row.value
            locModel
            { network = network
            , asset = row.currency
            }
        ++ [ ( "Currency"
             , Util.Csv.string <| String.toUpper row.currency
             )
           , ( "Height"
             , Util.Csv.int row.height
             )
           , ( "Timestamp_utc"
             , Locale.timestampNormal { locModel | zone = Time.utc } <| Data.timestampToPosix row.timestamp
             )
           , ( "Sending_address"
             , Util.Csv.string row.fromAddress
             )
           , ( "Sending_address_label"
             , tagSender
                |> Maybe.andThen .bestActor
                |> Maybe.withDefault ""
                |> Util.Csv.string
             )
           , ( "Receiving_address"
             , Util.Csv.string row.toAddress
             )
           , ( "Receiving_address_label"
             , tagReceiver
                |> Maybe.andThen .bestActor
                |> Maybe.withDefault ""
                |> Util.Csv.string
             )
           ]
