module Update.Pathfinder.AddressDetails exposing (browserGotClusterData, loadFirstPage, syncByAddress, update)

import Api.Data
import Api.Request.Addresses
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Components.PagedTable as PagedTable
import Config.DateRangePicker exposing (datePickerSettings)
import Config.Update as Update
import Dict exposing (Dict)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.DateRangePicker as DateRangePicker
import Init.Pathfinder.AddressDetails exposing (getExposedAssetsForAddress)
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Table.NeighborsTable as NeighborsTable
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Maybe.Extra
import Model.DateFilter exposing (DateFilterRaw)
import Model.Direction exposing (Direction(..))
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
import Tuple exposing (first, mapFirst, mapSecond, second)
import Tuple2 exposing (pairTo)
import Update.DateRangePicker as DateRangePicker
import Update.Pathfinder.Table.RelatedAddressesPubkeyTable as RelatedAddressesPubkeyTable
import Update.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Util exposing (and, n)
import Util.ThemedSelectBox as ThemedSelectBox


neighborsTableConfigWithMsg : (Direction -> Api.Data.NeighborAddresses -> Msg) -> Id -> Direction -> InfiniteTable.Config Effect
neighborsTableConfigWithMsg msg addressId dir =
    { fetch =
        \pagesize nextpage ->
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
    , triggerOffset = 100
    }


neighborsTableConfig : Id -> Direction -> InfiniteTable.Config Effect
neighborsTableConfig =
    neighborsTableConfigWithMsg GotNeighborsNextPageForAddressDetails


transactionTableConfig : TransactionTable.Model -> Id -> InfiniteTable.Config Effect
transactionTableConfig =
    transactionTableConfigWithMsg GotNextPageTxsForAddressDetails


transactionTableConfigWithMsg : (Api.Data.AddressTxs -> Msg) -> TransactionTable.Model -> Id -> InfiniteTable.Config Effect
transactionTableConfigWithMsg msg txs addressId =
    { fetch =
        \pagesize nextpage ->
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
    , triggerOffset = 100
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
                                InfiniteTable.loadFirstPage
                                    (neighborsTableConfigWithMsg GotNeighborsForAddressDetails model.address.id dir)
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

        NeighborsTableSubTableMsg dir pm ->
            getNeighborsTableAndSetter model dir
                |> Maybe.map
                    (\( tbl, setter ) ->
                        let
                            ( pt, cmd, eff ) =
                                InfiniteTable.update (neighborsTableConfig model.address.id dir) pm tbl
                        in
                        ( setter pt model
                        , CmdEffect (Cmd.map (NeighborsTableSubTableMsg dir >> Pathfinder.AddressDetailsMsg model.address.id) cmd)
                            :: Maybe.Extra.toList eff
                        )
                    )
                |> Maybe.withDefault (n model)

        GotNeighborsForAddressDetails dir neighbors ->
            getNeighborsTableAndSetter model dir
                |> Maybe.map
                    (\( tbl, setter ) ->
                        let
                            ( pt, cmd, eff ) =
                                InfiniteTable.setData
                                    (neighborsTableConfig model.address.id dir)
                                    NeighborsTable.filter
                                    neighbors.nextPage
                                    neighbors.neighbors
                                    tbl
                        in
                        ( setter pt model
                        , CmdEffect (Cmd.map (NeighborsTableSubTableMsg dir >> Pathfinder.AddressDetailsMsg model.address.id) cmd)
                            :: Maybe.Extra.toList eff
                        )
                    )
                |> Maybe.withDefault (n model)

        GotNeighborsNextPageForAddressDetails dir neighbors ->
            getNeighborsTableAndSetter model dir
                |> Maybe.map
                    (\( tbl, setter ) ->
                        let
                            ( pt, cmd, eff ) =
                                InfiniteTable.appendData
                                    (neighborsTableConfig model.address.id dir)
                                    NeighborsTable.filter
                                    neighbors.nextPage
                                    neighbors.neighbors
                                    tbl
                        in
                        ( setter pt model
                        , CmdEffect (Cmd.map (NeighborsTableSubTableMsg dir >> Pathfinder.AddressDetailsMsg model.address.id) cmd)
                            :: Maybe.Extra.toList eff
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
                            |> mapSecond Maybe.Extra.toList
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
                n { model | transactionsTableOpen = False }

            else
                openTransactionTable uc Nothing model

        GotNextPageTxsForAddressDetails txs ->
            model.txs
                |> RemoteData.map
                    (\txsTable ->
                        let
                            ( table, cmd, eff ) =
                                InfiniteTable.appendData
                                    (transactionTableConfig txsTable model.address.id)
                                    TransactionTable.filter
                                    txs.nextPage
                                    txs.addressTxs
                                    txsTable.table
                        in
                        ( table, eff )
                            |> mapFirst (flip s_table txsTable)
                            |> mapFirst (RemoteData.Success >> flip s_txs model)
                            |> mapSecond Maybe.Extra.toList
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

        GotTxsForAddressDetails ( min, max ) txs ->
            model.txs
                |> RemoteData.map
                    (\txsTable ->
                        let
                            drp =
                                txsTable.dateRangePicker
                        in
                        if Maybe.andThen .fromDate drp == min && Maybe.andThen .toDate drp == max then
                            let
                                ( table, cmd, eff ) =
                                    InfiniteTable.setData
                                        (transactionTableConfig txsTable model.address.id)
                                        TransactionTable.filter
                                        txs.nextPage
                                        txs.addressTxs
                                        txsTable.table
                            in
                            ( table, eff )
                                |> mapFirst (flip s_table txsTable)
                                |> mapFirst (RemoteData.Success >> flip s_txs model)
                                |> mapSecond Maybe.Extra.toList
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
                                                loadFirstPage

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
                        >> loadFirstPage
                    )
                |> RemoteData.withDefault (n model)

        ResetDateRangePicker ->
            model.txs
                |> RemoteData.map
                    (s_dateRangePicker Nothing
                        >> RemoteData.Success
                        >> flip s_txs model
                        >> loadFirstPage
                    )
                |> RemoteData.withDefault (n model)

        ResetTxDirectionFilter ->
            model.txs
                |> RemoteData.map
                    (s_direction Nothing
                        >> RemoteData.Success
                        >> flip s_txs model
                        >> loadFirstPage
                    )
                |> RemoteData.withDefault (n model)

        ResetTxAssetFilter ->
            model.txs
                |> RemoteData.map
                    (s_selectedAsset Nothing
                        >> RemoteData.Success
                        >> flip s_txs model
                        >> loadFirstPage
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
                        nm
                            |> (if show then
                                    flip updateRelatedAddressesTable
                                        (RelatedAddressesTable.loadFirstPage (RelatedAddressesTable.tableConfig ra))

                                else
                                    n
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
            RelatedAddressesTable.appendAddresses
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
                                table

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
                                    loadFirstPage

                                else
                                    n
                               )
                    )
                |> RemoteData.withDefault (n model)


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


loadFirstPage : Model -> ( Model, List Effect )
loadFirstPage model =
    model.txs
        |> RemoteData.map
            (\nt ->
                let
                    fromDate =
                        Maybe.andThen .fromDate nt.dateRangePicker

                    toDate =
                        Maybe.andThen .toDate nt.dateRangePicker

                    ( tableNew, eff ) =
                        nt.table
                            |> InfiniteTable.loadFirstPage
                                (transactionTableConfigWithMsg
                                    (GotTxsForAddressDetails ( fromDate, toDate ))
                                    nt
                                    model.address.id
                                )
                in
                ( { model
                    | txs =
                        RemoteData.Success { nt | table = tableNew }
                  }
                , Maybe.Extra.toList eff
                )
            )
        |> RemoteData.withDefault (n model)


updateDirectionFilter : Model -> Maybe Direction -> ( Model, List Effect )
updateDirectionFilter model dir =
    model.txs
        |> RemoteData.map (s_direction dir >> RemoteData.Success >> flip s_txs model >> loadFirstPage)
        |> RemoteData.withDefault (n model)



-- |> s_isTxFilterViewOpen False


browserGotClusterData : Id -> Api.Data.Entity -> Model -> ( Model, List Effect )
browserGotClusterData addressId entity model =
    let
        relatedAddresses =
            RelatedAddressesTable.init addressId entity
    in
    ( { model
        | relatedAddresses = RemoteData.Success relatedAddresses
        , relatedAddressesVisibleTable =
            if entity.noAddresses > 1 then
                MultiInputCluster

            else
                Pubkey
      }
    , []
    )


getNeighborsTableAndSetter : Model -> Direction -> Maybe ( InfiniteTable.Model Api.Data.NeighborAddress, InfiniteTable.Model Api.Data.NeighborAddress -> Model -> Model )
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
                                (TransactionTable.init uc network address.id data assets
                                    |> RemoteData.Success
                                )

                    related =
                        model.relatedAddresses
                            |> RemoteData.map (\_ -> model.relatedAddresses)
                            |> RemoteData.withDefault
                                (Id.initClusterId data.currency data.entity
                                    |> flip Dict.get clusters
                                    |> Maybe.map (RemoteData.map (RelatedAddressesTable.init address.id))
                                    |> Maybe.withDefault RemoteData.NotAsked
                                )

                    relatedPubkey =
                        model.relatedAddressesPubkey
                            |> RemoteData.map (\_ -> ( model.relatedAddressesPubkey, [] ))
                            |> RemoteData.withDefault
                                (let
                                    newTable =
                                        RelatedAddressesPubkeyTable.init address.id
                                 in
                                 ( RemoteData.Success newTable
                                 , [ RelatedAddressesPubkeyTable.loadFirstPage address.id ]
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
                        |> loadFirstPage
                )
                model.address.data
            |> RemoteData.withDefault (n model)
