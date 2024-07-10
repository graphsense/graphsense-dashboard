module Update.Pathfinder.AddressDetails exposing (showTransactionsTable, update)

import Basics.Extra exposing (flip)
import Config.Update as Update
import Effect exposing (n)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Direction exposing (Direction(..))
import Model.Pathfinder as Pathfinder exposing (Details(..))
import Model.Pathfinder.AddressDetails exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Table as PT
import Model.Pathfinder.Table.NeighborsTable as NeighborsTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import RecordSetter exposing (..)
import Update.DateRangePicker as DateRangePicker
import Update.Graph.Table exposing (UpdateSearchTerm(..))
import Update.Pathfinder.Table as PT


update : Update.Config -> Pathfinder.Model -> Msg -> Id -> Model -> ( Model, List Effect )
update uc pathfinderModel msg id model =
    case msg of
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
                    if not (tbl.table.nextpage == Nothing) then
                        ( (GotNeighborsForAddressDetails id dir >> AddressDetailsMsg)
                            |> Api.GetAddressNeighborsEffect
                                { currency = Id.network id
                                , address = Id.id id
                                , includeLabels = True
                                , onlyIds = Nothing
                                , isOutgoing = dir == Outgoing
                                , pagesize = tbl.itemsPerPage
                                , nextpage = tbl.table.nextpage
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
                    if not (model.txs.table.table.nextpage == Nothing) && not (PT.isNextPageLoaded model.txs.table) then
                        ( (GotNextPageTxsForAddressDetails id >> AddressDetailsMsg)
                            |> Api.GetAddressTxsEffect
                                { currency = Id.network id
                                , address = Id.id id
                                , direction = Nothing
                                , pagesize = model.txs.table.itemsPerPage
                                , nextpage = model.txs.table.table.nextpage
                                , order = model.txs.order
                                , minHeight = model.txMinBlock
                                , maxHeight = model.txMaxBlock
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

        UserClickedToggleTransactionTable ->
            not model.transactionsTableOpen
                |> showTransactionsTable id model

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

        GotTxsForAddressDetails responseId txs ->
            if responseId == id then
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
                                    , TransactionTable.loadFromDateBlock model.address.id newPicker.fromDate
                                        |> List.singleton
                                    )

                                else
                                    ( model.txMinBlock, [] )

                            ( txMaxBlock, endEff ) =
                                if newPicker.toDate /= dateRangePicker.toDate then
                                    ( Nothing
                                    , TransactionTable.loadToDateBlock model.address.id newPicker.toDate
                                        |> List.singleton
                                    )

                                else
                                    ( model.txMaxBlock, [] )
                        in
                        ( { model
                            | txs = s_dateRangePicker (Just newPicker) model.txs
                            , txMinBlock = txMinBlock
                            , txMaxBlock = txMaxBlock
                          }
                        , startEff ++ endEff
                        )
                    )
                |> Maybe.withDefault (n model)

        OpenDateRangePicker ->
            model.txs.dateRangePicker
                |> Maybe.map DateRangePicker.openPicker
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
            model.txs.dateRangePicker
                |> Maybe.map
                    (\_ ->
                        let
                            -- ( m2, eff ) =
                            --     updateDatePickerRangeBlockRange uc pathfinderModel id model Reset Reset
                            ( ft, teff ) =
                                TransactionTable.initWithoutFilter model.address uc.locale model.data
                        in
                        ( { model
                            | txs = ft
                          }
                        , teff
                        )
                    )
                |> Maybe.withDefault (n model)

        BrowserGotFromDateBlock _ blockAt ->
            updateDatePickerRangeBlockRange uc pathfinderModel id model (Set blockAt.beforeBlock) NoSet

        BrowserGotToDateBlock _ blockAt ->
            updateDatePickerRangeBlockRange uc pathfinderModel id model NoSet (Set blockAt.afterBlock)


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
                    model.txMinBlock

                Set x ->
                    Just x

        txmax =
            case txMaxBlock of
                Reset ->
                    Nothing

                NoSet ->
                    model.txMaxBlock

                Set x ->
                    Just x

        effects =
            case ( txmin, txmax ) of
                ( Just min, Just max ) ->
                    (GotTxsForAddressDetails id >> AddressDetailsMsg)
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
                    (GotTxsForAddressDetails id >> AddressDetailsMsg)
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
    in
    ( { model | txMinBlock = txmin, txMaxBlock = txmax }
    , effects
    )


showTransactionsTable : Id -> Model -> Bool -> ( Model, List Effect )
showTransactionsTable id model show =
    let
        eff =
            if List.isEmpty model.txs.table.table.data then
                (GotTxsForAddressDetails id >> AddressDetailsMsg)
                    |> Api.GetAddressTxsEffect
                        { currency = Id.network id
                        , address = Id.id id
                        , direction = Nothing
                        , pagesize = model.txs.table.itemsPerPage
                        , nextpage = model.txs.table.table.nextpage
                        , order = model.txs.order
                        , minHeight = Nothing
                        , maxHeight = Nothing
                        }
                    |> ApiEffect
                    |> List.singleton

            else
                []
    in
    ( { model | transactionsTableOpen = show }, [] )
