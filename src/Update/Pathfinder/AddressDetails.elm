module Update.Pathfinder.AddressDetails exposing (showTransactionsTable, update)

import Effect exposing (n)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Model.Direction exposing (Direction(..))
import Model.Graph.Table as GT
import Model.Pathfinder exposing (AddressDetailsViewState)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Table as PT
import Model.Pathfinder.Table.NeighborsTable as NeighborsTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder exposing (AddressDetailsMsg(..), Msg(..))
import RecordSetter exposing (..)
import Update.Graph.Table exposing (UpdateSearchTerm(..), appendData)


update : AddressDetailsMsg -> Id -> AddressDetailsViewState -> ( AddressDetailsViewState, List Effect )
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


showTransactionsTable : Id -> AddressDetailsViewState -> Bool -> ( AddressDetailsViewState, List Effect )
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
