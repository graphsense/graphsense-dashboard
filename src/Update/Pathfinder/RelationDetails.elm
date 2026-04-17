module Update.Pathfinder.RelationDetails exposing (gettersAndSetters, makeExportCSVConfig, update, updateAggEdge)

import Api.Data
import Api.Request.Addresses
import Basics.Extra exposing (flip)
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Components.TransactionFilter as TransactionFilter
import Config.Pathfinder exposing (numberOfRowsForCSVExport)
import Config.Update as Update
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..), effectToTracker)
import Init.Pathfinder.RelationDetails as Init
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id as Id exposing (Id, TxsFilterId(..))
import Model.Pathfinder.RelationDetails exposing (Model)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Model.Pathfinder.Tx exposing (getRawTimestampForRelationTx)
import Msg.Pathfinder as Pathfinder
import Msg.Pathfinder.RelationDetails as RelationDetails exposing (Msg(..))
import RecordSetter as Rs exposing (s_a2bTable, s_a2bTableOpen, s_b2aTable, s_b2aTableOpen, s_table)
import Table
import Time
import Tuple exposing (first, mapFirst, mapSecond, second)
import Util exposing (n)
import Util.Data as Data
import View.Graph.Table.AddresslinkTxsUtxoTable as AddresslinkTxsUtxoTable
import View.Graph.Table.TxsAccountTable as TxsAccountTable
import View.Locale as Locale


loadRelationTxs : (Bool -> Maybe String -> Api.Data.Links -> Msg) -> ( Id, Id ) -> Bool -> RelationTxsTable.Model -> Maybe ( String, Bool ) -> Int -> Maybe String -> Effect
loadRelationTxs msg id isA2b txTable sorting nrItems nextpage =
    let
        a =
            first id

        b =
            second id

        ( source, target ) =
            if isA2b then
                ( Id.id a, Id.id b )

            else
                ( Id.id b, Id.id a )

        settings =
            txTable.filter |> TransactionFilter.getSettings

        fromD =
            settings |> TransactionFilter.getDateRange |> Maybe.andThen first

        toD =
            settings |> TransactionFilter.getDateRange |> Maybe.andThen second
    in
    msg isA2b nextpage
        >> Pathfinder.RelationDetailsMsg id
        |> Api.GetAddresslinkTxsEffect
            { currency = Id.network a
            , source = source
            , target = target
            , minHeight = Nothing
            , maxHeight = Nothing
            , minDate = fromD
            , maxDate = toD
            , tokenCurrency = TransactionFilter.getSelectedAsset settings
            , order =
                sorting
                    |> Maybe.andThen
                        (\( col, isReversed ) ->
                            if col == RelationTxsTable.titleTimestamp then
                                if isReversed then
                                    Just Api.Request.Addresses.Order_Desc

                                else
                                    Just Api.Request.Addresses.Order_Asc

                            else
                                Nothing
                        )
            , nextpage = nextpage
            , pagesize = nrItems
            }
        |> ApiEffect


tableConfig : ( Id, Id ) -> Bool -> RelationTxsTable.Model -> InfiniteTable.Config Effect
tableConfig id isA2b txTable =
    { fetch = loadRelationTxs BrowserGotLinks id isA2b txTable
    , force = False
    , triggerOffset = 100
    , effectToTracker = effectToTracker
    , abort = Api.CancelEffect >> ApiEffect
    }


gettersAndSetters :
    Bool
    ->
        { getTable : Model -> RelationTxsTable.Model
        , setTable : RelationTxsTable.Model -> Model -> Model
        , getOpen : Model -> Bool
        , setOpen : Bool -> Model -> Model
        }
gettersAndSetters isA2b =
    if isA2b then
        { getTable = .a2bTable
        , setTable = s_a2bTable
        , getOpen = .a2bTableOpen
        , setOpen = s_a2bTableOpen
        }

    else
        { getTable = .b2aTable
        , setTable = s_b2aTable
        , getOpen = .b2aTableOpen
        , setOpen = s_b2aTableOpen
        }


updateAggEdge : Update.Config -> AggEdge -> Model -> Model
updateAggEdge _ edge model =
    let
        a2bSelect =
            edge.a2b
                |> Init.getExposedAssetsForNeighborWebData

        b2aSelect =
            edge.b2a
                |> Init.getExposedAssetsForNeighborWebData
    in
    { model
        | aggEdge = edge
        , a2bTable =
            model.a2bTable.filter
                |> (a2bSelect
                        |> Maybe.map TransactionFilter.withAssetSelectBox
                        |> Maybe.withDefault identity
                   )
                |> flip Rs.s_filter model.a2bTable
        , b2aTable =
            model.b2aTable.filter
                |> (b2aSelect
                        |> Maybe.map TransactionFilter.withAssetSelectBox
                        |> Maybe.withDefault identity
                   )
                |> flip Rs.s_filter model.b2aTable
    }


update : Update.Config -> ( Id, Id ) -> RelationDetails.Msg -> Model -> ( Model, List Effect )
update _ id msg model =
    case msg of
        TooltipMsg tm ->
            n model

        UserClickedToggleTable isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model

                isOpen =
                    gs.getOpen model

                conf =
                    tableConfig id isA2b tbl

                ( table, eff ) =
                    if isOpen then
                        InfiniteTable.abort conf tbl.table

                    else
                        tbl.table
                            |> InfiniteTable.gotoFirstPage conf
            in
            ( isOpen
                |> not
                |> flip gs.setOpen model
                |> gs.setTable (s_table table tbl)
            , eff
            )

        TableMsg isA2b tm ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model

                ( m, cmd, eff ) =
                    tbl
                        |> .table
                        |> InfiniteTable.update (tableConfig id isA2b tbl) tm
            in
            ( m, eff )
                |> mapFirst (flip s_table tbl)
                |> mapFirst (flip gs.setTable model)
                |> mapSecond
                    ((::)
                        (cmd
                            |> Cmd.map (TableMsg isA2b >> Pathfinder.RelationDetailsMsg id)
                            |> CmdEffect
                        )
                    )

        BrowserGotLinks isA2b fetchedPage data ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model

                setter =
                    if fetchedPage == Nothing then
                        InfiniteTable.setData

                    else
                        InfiniteTable.appendData

                ( table, cmd, eff ) =
                    tbl
                        |> .table
                        |> setter
                            (tableConfig id isA2b tbl)
                            RelationTxsTable.filter
                            data.nextPage
                            data.links
            in
            ( table, eff )
                |> mapFirst (flip s_table tbl)
                |> mapFirst (flip gs.setTable model)
                |> mapSecond
                    ((::)
                        (cmd
                            |> Cmd.map (TableMsg isA2b >> Pathfinder.RelationDetailsMsg id)
                            |> CmdEffect
                        )
                    )

        RelationDetails.NoOp ->
            n model

        UserClickedAllTxCheckboxInTable _ ->
            -- handled upstream
            n model

        UserClickedTxCheckboxInTable _ ->
            -- handled upstream
            n model

        RelationDetails.UserClickedTx _ ->
            -- handled upstream
            n model

        TransactionFilterMsg isA2b subMsg ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model

                newFilter =
                    TransactionFilter.update subMsg tbl.filter
                        |> (\( nf, _ ) ->
                                case subMsg of
                                    TransactionFilter.OpenDateRangePicker ->
                                        let
                                            focusDate =
                                                InfiniteTable.getTable tbl.table
                                                    |> .data
                                                    -- this is only try if data is sorted desc
                                                    |> List.head
                                                    |> Maybe.map getRawTimestampForRelationTx
                                                    |> Maybe.map ((*) 1000 >> Time.millisToPosix)
                                                    |> Maybe.withDefault model.rangeTo
                                        in
                                        TransactionFilter.setFocusDate focusDate nf

                                    _ ->
                                        nf
                           )

                changed =
                    TransactionFilter.hasChanged tbl.filter newFilter

                newTbl =
                    { tbl
                        | filter = newFilter
                    }
            in
            if changed then
                newTbl.table
                    |> InfiniteTable.loadFirstPage
                        (tableConfig id isA2b newTbl)
                    |> mapFirst (flip s_table newTbl)
                    |> mapFirst (flip gs.setTable model)
                    |> mapSecond
                        ((::)
                            (TransactionFilter.getSettings newFilter
                                |> Pathfinder.InternalChangedTxFilter (TxsFilterAggEdge isA2b id)
                                |> InternalEffect
                            )
                        )

            else
                model
                    |> gs.setTable newTbl
                    |> n

        ExportCSVMsg _ _ _ ->
            -- handled upstream
            n model

        BrowserGotLinksForExport _ _ _ ->
            -- handled upstream
            n model


makeExportCSVConfig : Update.Config -> Bool -> ( Id, Id ) -> RelationTxsTable.Model -> ExportCSV.Config Api.Data.Link Effect
makeExportCSVConfig uc isA2b id tbl =
    let
        nw =
            Id.network <| first id

        ( a, b ) =
            id

        ( source, target ) =
            if isA2b then
                ( Id.id a, Id.id b )

            else
                ( Id.id b, Id.id a )
    in
    ExportCSV.config
        { filename =
            Locale.interpolated uc.locale
                "Transactions-from-to"
                [ source
                , target
                , String.toUpper nw
                ]
        , toCsv =
            \tx ->
                if Data.isAccountLike nw then
                    case tx of
                        Api.Data.LinkLinkUtxo _ ->
                            Nothing

                        Api.Data.LinkTxAccount tx_ ->
                            TxsAccountTable.prepareCSV uc.locale nw tx_
                                |> List.map (mapFirst (Locale.string uc.locale))
                                |> Just

                else
                    case tx of
                        Api.Data.LinkLinkUtxo tx_ ->
                            AddresslinkTxsUtxoTable.prepareCSV uc.locale nw tx_
                                |> List.map (mapFirst (Locale.string uc.locale))
                                |> Just

                        Api.Data.LinkTxAccount _ ->
                            Nothing
        , cmdToEff =
            Cmd.map
                (ExportCSVMsg isA2b tbl
                    >> Pathfinder.RelationDetailsMsg id
                )
                >> CmdEffect
        , fetch =
            \nor ->
                loadRelationTxs
                    (\isA2b_ _ -> BrowserGotLinksForExport isA2b_ tbl)
                    id
                    isA2b
                    tbl
                    (tbl.table
                        |> InfiniteTable.getTable
                        |> .state
                        |> Table.getSortState
                        |> Just
                    )
                    nor
                    Nothing
        , numberOfRows = numberOfRowsForCSVExport
        , notificationToEff = ShowNotificationEffect
        }
