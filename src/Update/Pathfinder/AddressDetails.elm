module Update.Pathfinder.AddressDetails exposing (showTransactionsTable, update)

import Basics.Extra exposing (flip)
import Config.DateRangePicker exposing (datePickerSettings)
import Config.Update as Update
import Util exposing (n)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.DateRangePicker as DateRangePicker
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Direction exposing (Direction(..))
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.Address as Address
import Model.Pathfinder.AddressDetails exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.PagedTable as PT
import Model.Pathfinder.Table.NeighborsTable as NeighborsTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import RecordSetter exposing (..)
import Tuple exposing (mapFirst)
import Update.DateRangePicker as DateRangePicker
import Update.Graph.Table
import Update.Pathfinder.PagedTable as PT


update : Update.Config -> Pathfinder.Model -> Msg -> Id -> Model -> ( Model, List Effect )
update uc pathfinderModel msg id model =
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
                                ((GotNeighborsForAddressDetails id dir >> AddressDetailsMsg)
                                    |> Api.GetAddressNeighborsEffect
                                        { currency = Id.network id
                                        , address = Id.id id
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
                        ( (GotNeighborsForAddressDetails id dir >> AddressDetailsMsg)
                            |> Api.GetAddressNeighborsEffect
                                { currency = Id.network id
                                , address = Id.id id
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

        GotNeighborsForAddressDetails requestId dir neighbors ->
            if requestId == id then
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

            else
                n model

        UserClickedNextPageTransactionTable ->
            let
                ( eff, loading ) =
                    if (model.txs.table.table.nextpage /= Nothing) && not (PT.isNextPageLoaded model.txs.table) then
                        ( (GotNextPageTxsForAddressDetails id >> AddressDetailsMsg)
                            |> Api.GetAddressTxsEffect
                                { currency = Id.network id
                                , address = Id.id id
                                , direction = Nothing
                                , pagesize = model.txs.table.itemsPerPage
                                , nextpage = model.txs.table.table.nextpage
                                , order = model.txs.order
                                , minHeight = model.txs.txMinBlock
                                , maxHeight = model.txs.txMaxBlock
                                }
                            |> ApiEffect
                            |> List.singleton
                        , True
                        )

                    else
                        ( [], False )
            in
            if loading then
                ( { model
                    | txs =
                        PT.incPage model.txs.table
                            |> PT.setLoading loading
                            |> flip s_table model.txs
                  }
                , eff
                )

            else
                n
                    { model
                        | txs =
                            PT.incPage model.txs.table
                                |> flip s_table model.txs
                    }

        UserClickedPreviousPageTransactionTable ->
            ( { model | txs = PT.decPage model.txs.table |> flip s_table model.txs }, [] )

        UserClickedFirstPageTransactionTable ->
            ( { model | txs = PT.goToFirstPage model.txs.table |> flip s_table model.txs }, [] )

        UserClickedToggleTransactionTable ->
            not model.transactionsTableOpen
                |> showTransactionsTable model

        GotNextPageTxsForAddressDetails responseId txs ->
            if responseId == id then
                n
                    { model
                        | txs =
                            PT.appendData model.txs.table TransactionTable.filter txs.nextPage txs.addressTxs
                                |> flip s_table model.txs
                    }

            else
                n model

        GotTxsForAddressDetails responseId ( min, max ) txs ->
            if responseId == id && model.txs.txMinBlock == min && model.txs.txMaxBlock == max then
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
            updateDatePickerRangeBlockRange uc pathfinderModel id model (blockAt.beforeBlock |> Maybe.map Set |> Maybe.withDefault NoSet) NoSet

        BrowserGotToDateBlock _ blockAt ->
            updateDatePickerRangeBlockRange uc pathfinderModel id model NoSet (blockAt.afterBlock |> Maybe.map Set |> Maybe.withDefault NoSet)

        TableMsg _ ->
            n model


type SetOrNoSet x
    = Set x
    | NoSet
    | Reset


updateDatePickerRangeBlockRange : Update.Config -> Pathfinder.Model -> Id -> Model -> SetOrNoSet Int -> SetOrNoSet Int -> ( Model, List Effect )
updateDatePickerRangeBlockRange _ _ id model txMinBlock txMaxBlock =
    let
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
                    (GotTxsForAddressDetails id ( Just min, Just max ) >> AddressDetailsMsg)
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
                    (GotTxsForAddressDetails id ( Nothing, Nothing ) >> AddressDetailsMsg)
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
