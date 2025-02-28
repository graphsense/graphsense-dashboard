module Update.Pathfinder.AddressDetails exposing (browserGotClusterData, showTransactionsTable, update)

import Api.Data
import Basics.Extra exposing (flip)
import Config.DateRangePicker exposing (datePickerSettings)
import Config.Update as Update
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.DateRangePicker as DateRangePicker
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address as Address
import Model.Pathfinder.AddressDetails exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.PagedTable as PT
import Model.Pathfinder.Table.NeighborsTable as NeighborsTable
import Model.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import RecordSetter exposing (..)
import RemoteData
import Tuple exposing (mapFirst)
import Update.DateRangePicker as DateRangePicker
import Update.Graph.Table
import Update.Pathfinder.PagedTable as PT
import Update.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Util exposing (n)


update : Update.Config -> Msg -> Model -> ( Model, List Effect )
update uc msg model =
    case msg of
        UserClickedToggleTokenBalancesSelect ->
            ( model |> s_tokenBalancesOpen (not model.tokenBalancesOpen), [] )

        UserClickedToggleNeighborsTable ->
            let
                tables =
                    [ ( model.neighborsIncoming, Incoming ), ( model.neighborsOutgoing, Outgoing ) ]

                fetchFirstPageFn =
                    \( tbl, dir ) ->
                        if List.isEmpty tbl.table.data then
                            Just
                                ((GotNeighborsForAddressDetails dir >> AddressDetailsMsg model.addressId)
                                    |> Api.GetAddressNeighborsEffect
                                        { currency = Id.network model.addressId
                                        , address = Id.id model.addressId
                                        , includeLabels = True
                                        , onlyIds = Nothing
                                        , isOutgoing = dir == Outgoing
                                        , pagesize = tbl.itemsPerPage
                                        , nextpage = tbl.table.nextpage
                                        , includeActors = False
                                        }
                                    |> ApiEffect
                                )

                        else
                            Nothing

                eff =
                    List.filterMap fetchFirstPageFn tables
            in
            ( { model | neighborsTableOpen = not model.neighborsTableOpen }, eff )

        UserClickedNextPageNeighborsTable dir ->
            let
                ( tbl, setter ) =
                    case dir of
                        Incoming ->
                            ( model.neighborsIncoming, s_neighborsIncoming )

                        Outgoing ->
                            ( model.neighborsOutgoing, s_neighborsOutgoing )

                ( eff, loading ) =
                    if tbl.table.nextpage /= Nothing then
                        ( (GotNeighborsForAddressDetails dir >> AddressDetailsMsg model.addressId)
                            |> Api.GetAddressNeighborsEffect
                                { currency = Id.network model.addressId
                                , address = Id.id model.addressId
                                , includeLabels = True
                                , onlyIds = Nothing
                                , isOutgoing = dir == Outgoing
                                , pagesize = tbl.itemsPerPage
                                , nextpage = tbl.table.nextpage
                                , includeActors = False
                                }
                            |> ApiEffect
                            |> List.singleton
                        , True
                        )

                    else
                        ( [], False )
            in
            if loading then
                ( model |> setter ((PT.incPage >> PT.setLoading loading) tbl), eff )

            else
                n model

        UserClickedPreviousPageNeighborsTable dir ->
            let
                ( tbl, setter ) =
                    case dir of
                        Incoming ->
                            ( model.neighborsIncoming, s_neighborsIncoming )

                        Outgoing ->
                            ( model.neighborsOutgoing, s_neighborsOutgoing )
            in
            ( model |> setter (PT.decPage tbl), [] )

        GotNeighborsForAddressDetails dir neighbors ->
            n
                (case dir of
                    Incoming ->
                        { model
                            | neighborsIncoming = PT.appendData model.neighborsIncoming NeighborsTable.filter neighbors.nextPage neighbors.neighbors
                        }

                    Outgoing ->
                        { model
                            | neighborsOutgoing = PT.appendData model.neighborsOutgoing NeighborsTable.filter neighbors.nextPage neighbors.neighbors
                        }
                )

        UserClickedNextPageTransactionTable ->
            model.txs.table
                |> PT.nextPage
                    (\nextpage ->
                        (GotNextPageTxsForAddressDetails >> AddressDetailsMsg model.addressId)
                            |> Api.GetAddressTxsEffect
                                { currency = Id.network model.addressId
                                , address = Id.id model.addressId
                                , direction = Nothing
                                , pagesize = model.txs.table.itemsPerPage
                                , nextpage = nextpage
                                , order = model.txs.order
                                , minHeight = model.txs.txMinBlock
                                , maxHeight = model.txs.txMaxBlock
                                }
                            |> ApiEffect
                            |> List.singleton
                    )
                |> mapFirst (flip s_table model.txs)
                |> mapFirst (flip s_txs model)

        UserClickedPreviousPageTransactionTable ->
            ( { model | txs = PT.decPage model.txs.table |> flip s_table model.txs }, [] )

        UserClickedFirstPageTransactionTable ->
            ( { model | txs = PT.goToFirstPage model.txs.table |> flip s_table model.txs }, [] )

        UserClickedToggleTransactionTable ->
            not model.transactionsTableOpen
                |> showTransactionsTable model

        GotNextPageTxsForAddressDetails txs ->
            n
                { model
                    | txs =
                        PT.appendData model.txs.table TransactionTable.filter txs.nextPage txs.addressTxs
                            |> flip s_table model.txs
                }

        GotTxsForAddressDetails ( min, max ) txs ->
            if model.txs.txMinBlock == min && model.txs.txMaxBlock == max then
                n
                    { model
                        | txs =
                            s_nextpage txs.nextPage model.txs.table.table
                                |> Update.Graph.Table.setData
                                    TransactionTable.filter
                                    txs.addressTxs
                                |> flip s_table model.txs.table
                                |> flip s_table model.txs
                    }

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

        UserClickedPreviousPageRelatedAddressesTable ->
            updateRelatedAddressesTable model
                (\ra -> PT.decPage ra.table |> flip s_table ra |> n)

        UserClickedNextPageRelatedAddressesTable ->
            RelatedAddressesTable.loadNextPage
                |> updateRelatedAddressesTable model

        UserClickedFirstPageRelatedAddressesTable ->
            updateRelatedAddressesTable model
                (\ra -> PT.goToFirstPage ra.table |> flip s_table ra |> n)

        BrowserGotEntityAddressesForRelatedAddressesTable { nextPage, addresses } ->
            updateRelatedAddressesTable model
                (\ra ->
                    PT.appendData ra.table RelatedAddressesTable.filter nextPage addresses
                        |> flip s_table ra
                        |> n
                )


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

        effects =
            case ( txmin, txmax ) of
                ( Just min, Just max ) ->
                    (GotTxsForAddressDetails ( Just min, Just max ) >> AddressDetailsMsg model.addressId)
                        |> Api.GetAddressTxsEffect
                            { currency = Id.network id
                            , address = Id.id id
                            , direction = Nothing
                            , pagesize = model.txs.table.itemsPerPage
                            , nextpage = Nothing
                            , order = model.txs.order
                            , minHeight = Just min
                            , maxHeight = Just max
                            }
                        |> ApiEffect
                        |> List.singleton

                ( Nothing, Nothing ) ->
                    (GotTxsForAddressDetails ( Nothing, Nothing ) >> AddressDetailsMsg model.addressId)
                        |> Api.GetAddressTxsEffect
                            { currency = Id.network id
                            , address = Id.id id
                            , direction = Nothing
                            , pagesize = model.txs.table.itemsPerPage
                            , nextpage = Nothing
                            , order = model.txs.order
                            , minHeight = Nothing
                            , maxHeight = Nothing
                            }
                        |> ApiEffect
                        |> List.singleton

                _ ->
                    []

        txsNew =
            model.txs |> s_txMinBlock txmin |> s_txMaxBlock txmax
    in
    ( { model | txs = txsNew }
    , effects
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
