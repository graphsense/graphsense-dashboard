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
import Update.DateRangePicker as DateRangePicker
import Update.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Util exposing (n)


neighborsTableConfig : Id -> Direction -> PagedTable.Config Effect
neighborsTableConfig addressId dir =
    { fetch =
        Just
            (\pagesize nextpage ->
                GotNeighborsForAddressDetails dir
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
                    |> Api.GetAddressTxsEffect
                        { currency = Id.network addressId
                        , address = Id.id addressId
                        , direction = Nothing
                        , pagesize = pagesize
                        , nextpage = nextpage
                        , order = txs.order
                        , minHeight = txs.txMinBlock
                        , maxHeight = txs.txMaxBlock
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

        UserClickedToggleNeighborsTable ->
            let
                ( neighborsIncoming, eff1 ) =
                    PagedTable.loadFirstPage
                        (neighborsTableConfig model.addressId Incoming)
                        model.neighborsIncoming

                ( neighborsOutgoing, eff2 ) =
                    PagedTable.loadFirstPage
                        (neighborsTableConfig model.addressId Outgoing)
                        model.neighborsOutgoing
            in
            ( { model
                | neighborsTableOpen = not model.neighborsTableOpen
                , neighborsIncoming = neighborsIncoming
                , neighborsOutgoing = neighborsOutgoing
              }
            , Maybe.Extra.toList eff1 ++ Maybe.Extra.toList eff2
            )

        NeighborsTablePagedTableMsg dir pm ->
            let
                ( tbl, setter ) =
                    case dir of
                        Incoming ->
                            ( model.neighborsIncoming, s_neighborsIncoming )

                        Outgoing ->
                            ( model.neighborsOutgoing, s_neighborsOutgoing )

                ( pt, eff ) =
                    PagedTable.update (neighborsTableConfig model.addressId dir) pm tbl
            in
            ( setter pt model
            , Maybe.Extra.toList eff
            )

        GotNeighborsForAddressDetails dir neighbors ->
            let
                ( tbl, setter ) =
                    case dir of
                        Incoming ->
                            ( model.neighborsIncoming, s_neighborsIncoming )

                        Outgoing ->
                            ( model.neighborsOutgoing, s_neighborsOutgoing )

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

        TransactionsTablePagedTableMsg pm ->
            PagedTable.update (transactionTableConfig model.txs model.addressId) pm model.txs.table
                |> mapFirst (flip s_table model.txs)
                |> mapFirst (flip s_txs model)
                |> mapSecond Maybe.Extra.toList

        UserClickedToggleTransactionTable ->
            not model.transactionsTableOpen
                |> showTransactionsTable model

        GotNextPageTxsForAddressDetails txs ->
            PagedTable.appendData
                (transactionTableConfig model.txs model.addressId)
                TransactionTable.filter
                txs.nextPage
                txs.addressTxs
                model.txs.table
                |> mapFirst (flip s_table model.txs)
                |> mapFirst (flip s_txs model)
                |> mapSecond Maybe.Extra.toList

        GotTxsForAddressDetails ( min, max ) txs ->
            if model.txs.txMinBlock == min && model.txs.txMaxBlock == max then
                PagedTable.setData
                    (transactionTableConfig model.txs model.addressId)
                    TransactionTable.filter
                    txs.nextPage
                    txs.addressTxs
                    model.txs.table
                    |> mapFirst (flip s_table model.txs)
                    |> mapFirst (flip s_txs model)
                    |> mapSecond Maybe.Extra.toList

            else
                n model

        UpdateDateRangePicker subMsg ->
            model.txs.dateRangePicker
                |> Maybe.map
                    (\dateRangePicker ->
                        let
                            newPicker =
                                DateRangePicker.update subMsg dateRangePicker

                            ( txMinBlock, startEff ) =
                                if newPicker.fromDate /= dateRangePicker.fromDate then
                                    ( Nothing
                                    , TransactionTable.loadFromDateBlock model.addressId newPicker.fromDate
                                        |> List.singleton
                                    )

                                else
                                    ( model.txs.txMinBlock, [] )

                            ( txMaxBlock, endEff ) =
                                if newPicker.toDate /= dateRangePicker.toDate then
                                    ( Nothing
                                    , TransactionTable.loadToDateBlock model.addressId newPicker.toDate
                                        |> List.singleton
                                    )

                                else
                                    ( model.txs.txMaxBlock, [] )
                        in
                        ( { model
                            | txs = s_dateRangePicker (Just newPicker) model.txs |> s_txMinBlock txMinBlock |> s_txMaxBlock txMaxBlock
                          }
                        , startEff ++ endEff
                        )
                    )
                |> Maybe.withDefault (n model)

        OpenDateRangePicker ->
            let
                ( mn, mx ) =
                    Address.getActivityRange model.data
            in
            model.txs.dateRangePicker
                |> Maybe.withDefault
                    (datePickerSettings uc.locale mn mx
                        |> DateRangePicker.init UpdateDateRangePicker mn mx
                    )
                |> DateRangePicker.openPicker
                |> Just
                |> flip s_dateRangePicker model.txs
                |> flip s_txs model
                |> n

        CloseDateRangePicker ->
            model.txs.dateRangePicker
                |> Maybe.map DateRangePicker.closePicker
                |> flip s_dateRangePicker model.txs
                |> flip s_txs model
                |> n

        ResetDateRangePicker ->
            TransactionTable.initWithoutFilter model.addressId model.data
                |> mapFirst (flip s_txs model)

        BrowserGotFromDateBlock _ blockAt ->
            updateDatePickerRangeBlockRange uc model (blockAt.beforeBlock |> Maybe.map Set |> Maybe.withDefault NoSet) NoSet

        BrowserGotToDateBlock _ blockAt ->
            updateDatePickerRangeBlockRange uc model NoSet (blockAt.afterBlock |> Maybe.map Set |> Maybe.withDefault NoSet)

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


updateRelatedAddressesTable : Model -> (RelatedAddressesTable.Model -> ( RelatedAddressesTable.Model, List Effect )) -> ( Model, List Effect )
updateRelatedAddressesTable model upd =
    model.relatedAddresses
        |> RemoteData.toMaybe
        |> Maybe.map (upd >> mapFirst (RemoteData.Success >> flip s_relatedAddresses model))
        |> Maybe.withDefault (n model)


type SetOrNoSet x
    = Set x
    | NoSet
    | Reset


updateDatePickerRangeBlockRange : Update.Config -> Model -> SetOrNoSet Int -> SetOrNoSet Int -> ( Model, List Effect )
updateDatePickerRangeBlockRange _ model txMinBlock txMaxBlock =
    let
        id =
            model.addressId

        txmin =
            case txMinBlock of
                Reset ->
                    Nothing

                NoSet ->
                    model.txs.txMinBlock

                Set x ->
                    Just x

        txmax =
            case txMaxBlock of
                Reset ->
                    Nothing

                NoSet ->
                    model.txs.txMaxBlock

                Set x ->
                    Just x

        txsNew =
            model.txs
                |> s_txMinBlock txmin
                |> s_txMaxBlock txmax

        ( tableNew, eff ) =
            model.txs.table
                |> PagedTable.loadFirstPage
                    (transactionTableConfigWithMsg
                        (GotTxsForAddressDetails ( txmin, txmax ))
                        txsNew
                        id
                    )
    in
    ( { model
        | txs =
            { txsNew | table = tableNew }
      }
    , Maybe.Extra.toList eff
    )


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
