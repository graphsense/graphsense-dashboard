module Update.Pathfinder.AddressDetails exposing (showTransactionsTable, update)

import Effect exposing (n)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Direction exposing (Direction(..))
import Model.Graph.Table as GT
import Model.Pathfinder exposing (Details(..))
import Model.Pathfinder.AddressDetails exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Table as PT
import Model.Pathfinder.Table.NeighborsTable as NeighborsTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import RecordSetter exposing (..)
import Update.DateRangePicker as DateRangePicker
import Update.Graph.Table exposing (UpdateSearchTerm(..), appendData)


update : Msg -> Id -> Model -> ( Model, List Effect )
update msg id model =
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
                                | neighborsIncoming = appendPagedTableData model.neighborsIncoming NeighborsTable.filter neighbors.nextPage neighbors.neighbors
                            }

                        Outgoing ->
                            { model
                                | neighborsOutgoing = appendPagedTableData model.neighborsOutgoing NeighborsTable.filter neighbors.nextPage neighbors.neighbors
                            }
                    )

            else
                n model

        UserClickedNextPageTransactionTable ->
            let
                ( eff, loading ) =
                    if not (model.txs.table.nextpage == Nothing) then
                        ( (GotTxsForAddressDetails id >> AddressDetailsMsg)
                            |> Api.GetAddressTxsEffect
                                { currency = Id.network id
                                , address = Id.id id
                                , direction = Nothing
                                , pagesize = model.txs.itemsPerPage
                                , nextpage = model.txs.table.nextpage
                                , order = Nothing
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
                ( { model | txs = (PT.incPage >> PT.setLoading loading) model.txs }, eff )

            else
                n model

        UserClickedPreviousPageTransactionTable ->
            ( { model | txs = PT.decPage model.txs }, [] )

        UserClickedToggleTransactionTable ->
            not model.transactionsTableOpen
                |> showTransactionsTable id model

        GotTxsForAddressDetails responseId txs ->
            if responseId == id then
                n
                    { model
                        | txs = appendPagedTableData model.txs TransactionTable.filter txs.nextPage txs.addressTxs
                    }

            else
                n model

        UpdateDateRangePicker subMsg ->
            let
                newPicker =
                    DateRangePicker.update subMsg model.dateRangePicker

                ( txMinBlock, startEff ) =
                    if newPicker.fromDate /= model.dateRangePicker.fromDate then
                        newPicker.fromDate
                            |> Maybe.map
                                (\st ->
                                    ( Nothing
                                    , BrowserGotFromDateBlock st
                                        >> AddressDetailsMsg
                                        |> Api.GetBlockByDateEffect
                                            { currency = Id.network id
                                            , datetime = st
                                            }
                                        |> ApiEffect
                                        |> List.singleton
                                    )
                                )
                            |> Maybe.withDefault ( model.txMinBlock, [] )

                    else
                        ( model.txMinBlock, [] )

                ( txMaxBlock, endEff ) =
                    if newPicker.toDate /= model.dateRangePicker.toDate then
                        newPicker.toDate
                            |> Maybe.map
                                (\et ->
                                    ( Nothing
                                    , BrowserGotToDateBlock et
                                        >> AddressDetailsMsg
                                        |> Api.GetBlockByDateEffect
                                            { currency = Id.network id
                                            , datetime = et
                                            }
                                        |> ApiEffect
                                        |> List.singleton
                                    )
                                )
                            |> Maybe.withDefault ( model.txMaxBlock, [] )

                    else
                        ( model.txMaxBlock, [] )
            in
            ( { model
                | dateRangePicker = newPicker
                , txMinBlock = txMinBlock
                , txMaxBlock = txMaxBlock
              }
            , startEff ++ endEff
            )

        OpenDateRangePicker ->
            n { model | dateRangePicker = DateRangePicker.openPicker model.dateRangePicker }

        CloseDateRangePicker ->
            n { model | dateRangePicker = DateRangePicker.closePicker model.dateRangePicker }

        ResetDateRangePicker ->
            let
                ( m2, eff ) =
                    updateDatePickerRangeBlockRange id model Reset Reset
            in
            ( { m2
                | dateRangePicker = DateRangePicker.resetPicker model.dateRangePicker
              }
            , eff
            )

        BrowserGotFromDateBlock _ blockAt ->
            updateDatePickerRangeBlockRange id model (Set blockAt.beforeBlock) NoSet

        BrowserGotToDateBlock _ blockAt ->
            updateDatePickerRangeBlockRange id model NoSet (Set blockAt.afterBlock)


type SetOrNoSet x
    = Set x
    | NoSet
    | Reset


updateDatePickerRangeBlockRange : Id -> Model -> SetOrNoSet Int -> SetOrNoSet Int -> ( Model, List Effect )
updateDatePickerRangeBlockRange id model txMinBlock txMaxBlock =
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
                            , pagesize = model.txs.itemsPerPage
                            , nextpage = Nothing
                            , order = Nothing
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
                            , pagesize = model.txs.itemsPerPage
                            , nextpage = Nothing
                            , order = Nothing
                            , minHeight = Nothing
                            , maxHeight = Nothing
                            }
                        |> ApiEffect
                        |> List.singleton

                _ ->
                    []

        txsnew =
            case ( txmin, txmax ) of
                ( Just _, Just _ ) ->
                    TransactionTable.init Nothing

                ( Nothing, Nothing ) ->
                    TransactionTable.init Nothing

                _ ->
                    model.txs
    in
    ( { model | txMinBlock = txmin, txMaxBlock = txmax, txs = txsnew }
    , effects
    )


showTransactionsTable : Id -> Model -> Bool -> ( Model, List Effect )
showTransactionsTable id model show =
    let
        eff =
            if List.isEmpty model.txs.table.data then
                (GotTxsForAddressDetails id >> AddressDetailsMsg)
                    |> Api.GetAddressTxsEffect
                        { currency = Id.network id
                        , address = Id.id id
                        , direction = Nothing
                        , pagesize = model.txs.itemsPerPage
                        , nextpage = model.txs.table.nextpage
                        , order = Nothing
                        , minHeight = Nothing
                        , maxHeight = Nothing
                        }
                    |> ApiEffect
                    |> List.singleton

            else
                []
    in
    ( { model | transactionsTableOpen = show }, eff )


appendPagedTableData : PT.PagedTable p -> GT.Filter p -> Maybe String -> List p -> PT.PagedTable p
appendPagedTableData pt f nextPage data =
    { pt
        | table =
            appendData f data pt.table
                |> s_nextpage nextPage
                |> s_loading False
    }
