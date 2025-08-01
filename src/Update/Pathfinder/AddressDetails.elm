module Update.Pathfinder.AddressDetails exposing (browserGotClusterData, showTransactionsTable, update)

import Api.Data
import Basics.Extra exposing (flip)
import Config.DateRangePicker exposing (datePickerSettings)
import Config.Update as Update
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.DateRangePicker as DateRangePicker
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Maybe.Extra
import Model.Direction exposing (Direction(..))
import Model.Locale as Locale
import Model.Pathfinder.Address as Address
import Model.Pathfinder.AddressDetails exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Table.NeighborsTable as NeighborsTable
import Model.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder as Pathfinder
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import PagedTable
import RecordSetter exposing (..)
import RemoteData
import Set
import Tuple exposing (mapFirst, mapSecond)
import Tuple2 exposing (pairTo)
import Update.DateRangePicker as DateRangePicker
import Update.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Util exposing (n)
import Util.ThemedSelectBox as ThemedSelectBox


neighborsTableConfigWithMsg : (Direction -> Api.Data.NeighborAddresses -> Msg) -> Id -> Direction -> PagedTable.Config Effect
neighborsTableConfigWithMsg msg addressId dir =
    { fetch =
        Just
            (\pagesize nextpage ->
                msg dir
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
            )
    }


neighborsTableConfig : Id -> Direction -> PagedTable.Config Effect
neighborsTableConfig =
    neighborsTableConfigWithMsg GotNeighborsNextPageForAddressDetails


transactionTableConfig : TransactionTable.Model -> Id -> PagedTable.Config Effect
transactionTableConfig =
    transactionTableConfigWithMsg GotNextPageTxsForAddressDetails


transactionTableConfigWithMsg : (Api.Data.AddressTxs -> Msg) -> TransactionTable.Model -> Id -> PagedTable.Config Effect
transactionTableConfigWithMsg msg txs addressId =
    { fetch =
        Just
            (\pagesize nextpage ->
                msg
                    >> Pathfinder.AddressDetailsMsg addressId
                    |> Api.GetAddressTxsByDateEffect
                        { currency = Id.network addressId
                        , address = Id.id addressId
                        , direction = txs.direction
                        , pagesize = pagesize
                        , nextpage = nextpage
                        , order = txs.order
                        , tokenCurrency = txs.selectedAsset
                        , minDate = txs.dateRangePicker |> Maybe.andThen .fromDate
                        , maxDate = txs.dateRangePicker |> Maybe.andThen .toDate
                        }
                    |> ApiEffect
            )
    }


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
                    (\( tbl, setter ) ->
                        let
                            ( tblNew, eff1 ) =
                                PagedTable.loadFirstPage
                                    (neighborsTableConfigWithMsg GotNeighborsForAddressDetails model.addressId dir)
                                    tbl
                        in
                        ( { model
                            | incomingNeighborsTableOpen =
                                if dir == Incoming then
                                    not model.incomingNeighborsTableOpen

                                else
                                    model.incomingNeighborsTableOpen
                            , outgoingNeighborsTableOpen =
                                if dir == Outgoing then
                                    not model.outgoingNeighborsTableOpen

                                else
                                    model.outgoingNeighborsTableOpen
                          }
                            |> setter tblNew
                        , Maybe.Extra.toList eff1
                        )
                    )
                |> Maybe.withDefault (n model)

        NeighborsTablePagedTableMsg dir pm ->
            getNeighborsTableAndSetter model dir
                |> Maybe.map
                    (\( tbl, setter ) ->
                        let
                            ( pt, eff ) =
                                PagedTable.update (neighborsTableConfig model.addressId dir) pm tbl
                        in
                        ( setter pt model
                        , Maybe.Extra.toList eff
                        )
                    )
                |> Maybe.withDefault (n model)

        GotNeighborsForAddressDetails dir neighbors ->
            getNeighborsTableAndSetter model dir
                |> Maybe.map
                    (\( tbl, setter ) ->
                        let
                            ( pt, eff ) =
                                PagedTable.setData
                                    (neighborsTableConfig model.addressId dir)
                                    NeighborsTable.filter
                                    neighbors.nextPage
                                    neighbors.neighbors
                                    tbl
                        in
                        ( setter pt model
                        , Maybe.Extra.toList eff
                        )
                    )
                |> Maybe.withDefault (n model)

        GotNeighborsNextPageForAddressDetails dir neighbors ->
            getNeighborsTableAndSetter model dir
                |> Maybe.map
                    (\( tbl, setter ) ->
                        let
                            ( pt, eff ) =
                                PagedTable.appendData
                                    (neighborsTableConfig model.addressId dir)
                                    NeighborsTable.filter
                                    neighbors.nextPage
                                    neighbors.neighbors
                                    tbl
                        in
                        ( setter pt model
                        , Maybe.Extra.toList eff
                        )
                    )
                |> Maybe.withDefault (n model)

        TransactionsTablePagedTableMsg pm ->
            model.txs
                |> RemoteData.map
                    (\txs ->
                        PagedTable.update (transactionTableConfig txs model.addressId) pm txs.table
                            |> mapFirst (flip s_table txs)
                            |> mapFirst (RemoteData.Success >> flip s_txs model)
                            |> mapSecond Maybe.Extra.toList
                    )
                |> RemoteData.withDefault (n model)

        UserClickedToggleTransactionTable ->
            not model.transactionsTableOpen
                |> showTransactionsTable model

        GotNextPageTxsForAddressDetails txs ->
            model.txs
                |> RemoteData.map
                    (\txsTable ->
                        PagedTable.appendData
                            (transactionTableConfig txsTable model.addressId)
                            TransactionTable.filter
                            txs.nextPage
                            txs.addressTxs
                            txsTable.table
                            |> mapFirst (flip s_table txsTable)
                            |> mapFirst (RemoteData.Success >> flip s_txs model)
                            |> mapSecond Maybe.Extra.toList
                    )
                |> RemoteData.withDefault (n model)

        GotTxsForAddressDetails ( min, max ) txs ->
            model.txs
                |> RemoteData.map
                    (\txsTable ->
                        let
                            drp =
                                txsTable.dateRangePicker
                        in
                        if Maybe.andThen .fromDate drp == min && Maybe.andThen .toDate drp == max then
                            PagedTable.setData
                                (transactionTableConfig txsTable model.addressId)
                                TransactionTable.filter
                                txs.nextPage
                                txs.addressTxs
                                txsTable.table
                                |> mapFirst (flip s_table txsTable)
                                |> mapFirst (RemoteData.Success >> flip s_txs model)
                                |> mapSecond Maybe.Extra.toList

                        else
                            n model
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

                                        eff =
                                            if newPicker.fromDate /= Nothing && newPicker.fromDate /= dateRangePicker.fromDate then
                                                TransactionTable.loadTxs model.addressId Nothing newPicker.fromDate newPicker.toDate txsTable.selectedAsset

                                            else
                                                []

                                        np =
                                            if List.length eff > 0 then
                                                newPicker |> DateRangePicker.closePicker

                                            else
                                                newPicker
                                    in
                                    ( { model
                                        | txs = s_dateRangePicker (Just np) txsTable |> RemoteData.Success
                                      }
                                    , eff
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
                    model.data
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
            updateDirectionFilter uc model Nothing

        TxTableFilterShowIncomingTxOnly ->
            updateDirectionFilter uc model (Just Incoming)

        TxTableFilterShowOutgoingTxOnly ->
            updateDirectionFilter uc model (Just Outgoing)

        ResetAllTxFilters ->
            model.data
                |> RemoteData.map
                    (\data ->
                        TransactionTable.initWithFilter
                            model.addressId
                            data
                            Nothing
                            Nothing
                            Nothing
                            (Locale.getTokenTickers uc.locale (Id.network model.addressId))
                            |> mapFirst (RemoteData.Success >> flip s_txs model)
                    )
                |> RemoteData.withDefault (n model)

        ResetDateRangePicker ->
            RemoteData.map2
                (\data txs ->
                    TransactionTable.initWithFilter
                        model.addressId
                        data
                        Nothing
                        txs.direction
                        txs.selectedAsset
                        (Locale.getTokenTickers uc.locale (Id.network model.addressId))
                        |> mapFirst (RemoteData.Success >> flip s_txs model)
                )
                model.data
                model.txs
                |> RemoteData.withDefault (n model)

        ResetTxDirectionFilter ->
            RemoteData.map2
                (\data txs ->
                    TransactionTable.initWithFilter
                        model.addressId
                        data
                        txs.dateRangePicker
                        Nothing
                        txs.selectedAsset
                        (Locale.getTokenTickers uc.locale (Id.network model.addressId))
                        |> mapFirst (RemoteData.Success >> flip s_txs model)
                )
                model.data
                model.txs
                |> RemoteData.withDefault (n model)

        ResetTxAssetFilter ->
            RemoteData.map2
                (\data txs ->
                    TransactionTable.initWithFilter
                        model.addressId
                        data
                        txs.dateRangePicker
                        txs.direction
                        Nothing
                        (Locale.getTokenTickers uc.locale (Id.network model.addressId))
                        |> mapFirst (RemoteData.Success >> flip s_txs model)
                )
                model.data
                model.txs
                |> RemoteData.withDefault (n model)

        TableMsg _ ->
            n model

        RelatedAddressesTableMsg _ ->
            n model

        UserClickedToggleRelatedAddressesTable ->
            n { model | relatedAddressesTableOpen = not model.relatedAddressesTableOpen }

        RelatedAddressesTablePagedTableMsg pm ->
            (\rm ->
                RelatedAddressesTable.updateTable
                    (PagedTable.update (RelatedAddressesTable.tableConfig rm) pm)
                    rm
            )
                |> updateRelatedAddressesTable model

        BrowserGotEntityAddressesForRelatedAddressesTable { nextPage, addresses } ->
            RelatedAddressesTable.appendAddresses nextPage addresses
                |> updateRelatedAddressesTable model

        UserClickedTx _ ->
            n model

        UserClickedAllTxCheckboxInTable ->
            n model

        UserClickedTxCheckboxInTable _ ->
            n model

        UserClickedAddressCheckboxInTable _ ->
            n model

        NoOp ->
            n model

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
                    >> Pathfinder.AddressDetailsMsg model.addressId
                    |> Api.BulkGetAddressEffect
                        { currency = currency
                        , addresses = addressesToLoad
                        }
                    |> ApiEffect
                    |> List.singleton
                )

            else
                RelatedAddressesTable.appendTaggedAddresses tags.nextPage []
                    |> updateRelatedAddressesTable model

        BrowserGotAddressesForTags nextpage addresses ->
            RelatedAddressesTable.appendTaggedAddresses nextpage addresses
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
                        if oldTxs == newTxs then
                            n { model | txs = RemoteData.Success newTxs }

                        else
                            updateTable uc model newTxs
                    )
                |> RemoteData.withDefault (n model)


updateRelatedAddressesTable : Model -> (RelatedAddressesTable.Model -> ( RelatedAddressesTable.Model, List Effect )) -> ( Model, List Effect )
updateRelatedAddressesTable model upd =
    model.relatedAddresses
        |> RemoteData.toMaybe
        |> Maybe.map (upd >> mapFirst (RemoteData.Success >> flip s_relatedAddresses model))
        |> Maybe.withDefault (n model)


updateTable : Update.Config -> Model -> TransactionTable.Model -> ( Model, List Effect )
updateTable _ model nt =
    let
        fromDate =
            Maybe.andThen .fromDate nt.dateRangePicker

        toDate =
            Maybe.andThen .toDate nt.dateRangePicker

        ( tableNew, eff ) =
            nt.table
                |> PagedTable.goToFirstPage
                |> PagedTable.loadFirstPage
                    (transactionTableConfigWithMsg
                        (GotTxsForAddressDetails ( fromDate, toDate ))
                        nt
                        model.addressId
                    )
    in
    ( { model
        | txs =
            RemoteData.Success { nt | table = tableNew }
      }
    , Maybe.Extra.toList eff
    )


updateDirectionFilter : Update.Config -> Model -> Maybe Direction -> ( Model, List Effect )
updateDirectionFilter uc model dir =
    model.txs
        |> RemoteData.map (s_direction dir >> updateTable uc model)
        |> RemoteData.withDefault (n model)



-- |> s_isTxFilterViewOpen False


showTransactionsTable : Model -> Bool -> ( Model, List Effect )
showTransactionsTable model show =
    ( { model | transactionsTableOpen = show }, [] )


browserGotClusterData : Id -> Api.Data.Entity -> Model -> ( Model, List Effect )
browserGotClusterData addressId entity model =
    let
        ( relatedAddresses, eff ) =
            RelatedAddressesTable.init addressId entity
    in
    ( { model
        | relatedAddresses = RemoteData.Success relatedAddresses
      }
    , eff
    )


getNeighborsTableAndSetter : Model -> Direction -> Maybe ( PagedTable.Model Api.Data.NeighborAddress, PagedTable.Model Api.Data.NeighborAddress -> Model -> Model )
getNeighborsTableAndSetter model dir =
    case dir of
        Incoming ->
            model.neighborsIncoming
                |> RemoteData.toMaybe
                |> Maybe.map (pairTo (RemoteData.Success >> s_neighborsIncoming))

        Outgoing ->
            model.neighborsOutgoing
                |> RemoteData.toMaybe
                |> Maybe.map (pairTo (RemoteData.Success >> s_neighborsOutgoing))
