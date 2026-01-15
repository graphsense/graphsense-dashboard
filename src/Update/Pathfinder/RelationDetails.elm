module Update.Pathfinder.RelationDetails exposing (gettersAndSetters, makeExportCSVConfig, update, updateAggEdge)

import Api.Data
import Api.Request.Addresses
import Basics.Extra exposing (flip)
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Config.DateRangePicker exposing (datePickerSettings)
import Config.Pathfinder exposing (numberOfRowsForCSVExport)
import Config.Update as Update
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..), effectToTracker)
import Init.DateRangePicker as DateRangePicker
import Init.Pathfinder.RelationDetails as Init
import Model.Locale as Locale
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.RelationDetails exposing (Model)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Model.Pathfinder.Tx exposing (getRawTimestampForRelationTx)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.RelationDetails as RelationDetails exposing (Msg(..))
import RecordSetter as Rs exposing (s_a2bTable, s_a2bTableOpen, s_assetSelectBox, s_b2aTable, s_b2aTableOpen, s_dateRangePicker, s_isTxFilterViewOpen, s_selectedAsset, s_table)
import Table
import Time
import Tuple exposing (first, mapFirst, mapSecond, second)
import Update.DateRangePicker as DateRangePicker
import Util exposing (n)
import Util.Data as Data
import Util.ThemedSelectBox as ThemedSelectBox
import View.Graph.Table.AddresslinkTxsUtxoTable as AddresslinkTxsUtxoTable
import View.Graph.Table.TxsAccountTable as TxsAccountTable
import View.Locale as Locale


loadRelationTxs : (Bool -> Maybe String -> Api.Data.Links -> Msg) -> ( Id, Id ) -> Bool -> RelationTxsTable.Model Msg -> Maybe ( String, Bool ) -> Int -> Maybe String -> Effect
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

        fromD =
            txTable.dateRangePicker |> Maybe.andThen .fromDate

        toD =
            txTable.dateRangePicker |> Maybe.andThen .toDate
    in
    msg isA2b nextpage
        >> RelationDetailsMsg id
        |> Api.GetAddresslinkTxsEffect
            { currency = Id.network a
            , source = source
            , target = target
            , minHeight = Nothing
            , maxHeight = Nothing
            , minDate = fromD
            , maxDate = toD
            , tokenCurrency = txTable.selectedAsset
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


tableConfig : ( Id, Id ) -> Bool -> RelationTxsTable.Model Msg -> InfiniteTable.Config Effect
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
        { getTable : Model -> RelationTxsTable.Model Msg
        , setTable : RelationTxsTable.Model Msg -> Model -> Model
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
updateAggEdge uc edge model =
    let
        a2bSelect =
            edge.a2b
                |> Init.getExposedAssetsForNeighborWebData (Locale.getTokenTickersAndBase uc.locale (edge.a |> Id.network))
                |> List.map Just
                |> (::) Nothing

        b2aSelect =
            edge.b2a
                |> Init.getExposedAssetsForNeighborWebData (Locale.getTokenTickersAndBase uc.locale (edge.b |> Id.network))
                |> List.map Just
                |> (::) Nothing
    in
    { model
        | aggEdge = edge
        , a2bTable =
            model.a2bTable.assetSelectBox
                |> ThemedSelectBox.updateOptions a2bSelect
                |> flip Rs.s_assetSelectBox model.a2bTable
        , b2aTable =
            model.b2aTable.assetSelectBox
                |> ThemedSelectBox.updateOptions b2aSelect
                |> flip Rs.s_assetSelectBox model.b2aTable
    }


update : Update.Config -> ( Id, Id ) -> ( Time.Posix, Time.Posix ) -> RelationDetails.Msg -> Model -> ( Model, List Effect )
update uc id ( rangeFrom, rangeTo ) msg model =
    case msg of
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
                            |> Cmd.map (TableMsg isA2b >> RelationDetailsMsg id)
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
                            |> Cmd.map (TableMsg isA2b >> RelationDetailsMsg id)
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

        ToggleTxFilterView isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model
            in
            tbl.dateRangePicker
                |> flip s_dateRangePicker tbl
                |> s_isTxFilterViewOpen (not tbl.isTxFilterViewOpen)
                |> flip gs.setTable model
                |> n

        CloseTxFilterView isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model
            in
            tbl
                |> s_isTxFilterViewOpen False
                |> flip gs.setTable model
                |> n

        OpenDateRangePicker isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model

                focusDate =
                    InfiniteTable.getTable tbl.table
                        |> .data
                        -- this is only try if data is sorted desc
                        |> List.head
                        |> Maybe.map getRawTimestampForRelationTx
                        |> Maybe.map ((*) 1000 >> Time.millisToPosix)
                        |> Maybe.withDefault rangeTo
            in
            tbl.dateRangePicker
                |> Maybe.withDefault
                    (datePickerSettings uc.locale rangeFrom focusDate
                        |> DateRangePicker.init (UpdateDateRangePicker isA2b) focusDate Nothing Nothing
                    )
                |> DateRangePicker.openPicker
                |> Just
                |> flip s_dateRangePicker tbl
                |> flip gs.setTable model
                |> n

        UpdateDateRangePicker isA2b subMsg ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model
            in
            tbl.dateRangePicker
                |> Maybe.map
                    (\dateRangePicker ->
                        let
                            newPicker =
                                DateRangePicker.update subMsg dateRangePicker

                            dateRangeChanged =
                                (newPicker.toDate /= Nothing && newPicker.toDate /= dateRangePicker.toDate) || (newPicker.fromDate /= Nothing && newPicker.fromDate /= dateRangePicker.fromDate)

                            --&& ((newPicker.toDate |> Maybe.Extra.isJust) && (newPicker.fromDate |> Maybe.Extra.isJust))
                            picker =
                                if dateRangeChanged then
                                    newPicker |> DateRangePicker.closePicker

                                else
                                    newPicker

                            udateTbl =
                                tbl |> s_dateRangePicker (Just picker)

                            ( ntbl, eff ) =
                                if dateRangeChanged then
                                    udateTbl
                                        |> .table
                                        |> InfiniteTable.loadFirstPage
                                            (tableConfig id isA2b udateTbl)

                                else
                                    ( udateTbl.table, [] )
                        in
                        ( model |> gs.setTable (udateTbl |> s_table ntbl)
                        , if dateRangeChanged then
                            eff

                          else
                            []
                        )
                    )
                |> Maybe.withDefault (n model)

        CloseDateRangePicker isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model
            in
            tbl.dateRangePicker
                |> Maybe.map DateRangePicker.closePicker
                |> flip s_dateRangePicker tbl
                |> flip gs.setTable model
                |> n

        ResetDateRangePicker isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                oldTable =
                    gs.getTable model

                tbl =
                    oldTable
                        |> s_dateRangePicker Nothing

                ( table, eff ) =
                    tbl
                        |> .table
                        |> InfiniteTable.loadFirstPage
                            (tableConfig id isA2b tbl)
            in
            ( model |> gs.setTable (tbl |> s_table table)
            , eff
            )

        ResetAllTxFilters isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                oldTable =
                    gs.getTable model

                tbl =
                    oldTable
                        |> s_selectedAsset Nothing
                        |> s_dateRangePicker Nothing

                ( table, eff ) =
                    tbl
                        |> .table
                        |> InfiniteTable.loadFirstPage
                            (tableConfig id isA2b tbl)
            in
            ( model |> gs.setTable (tbl |> s_table table)
            , eff
            )

        ResetTxAssetFilter isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                oldTable =
                    gs.getTable model

                tbl =
                    oldTable
                        |> s_selectedAsset Nothing

                ( table, eff ) =
                    tbl
                        |> .table
                        |> InfiniteTable.loadFirstPage
                            (tableConfig id isA2b tbl)
            in
            ( model |> gs.setTable (tbl |> s_table table)
            , eff
            )

        TxTableAssetSelectBoxMsg isA2b ms ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model

                ( newSelect, outMsg ) =
                    ThemedSelectBox.update ms tbl.assetSelectBox

                newTxs =
                    tbl
                        |> s_assetSelectBox newSelect
                        |> s_selectedAsset
                            (case outMsg of
                                ThemedSelectBox.Selected sel ->
                                    sel

                                _ ->
                                    tbl.selectedAsset
                            )
            in
            if tbl.selectedAsset == newTxs.selectedAsset then
                -- no change
                n (model |> gs.setTable newTxs)

            else
                let
                    ( ntbl, eff ) =
                        newTxs
                            |> .table
                            |> InfiniteTable.loadFirstPage
                                (tableConfig id isA2b newTxs)
                in
                ( newTxs
                    |> s_table ntbl
                    |> flip gs.setTable model
                , eff
                )

        ExportCSVMsg _ _ _ ->
            -- handled upstream
            n model

        BrowserGotLinksForExport _ _ _ ->
            -- handled upstream
            n model


makeExportCSVConfig : Update.Config -> Bool -> ( Id, Id ) -> RelationTxsTable.Model Msg -> ExportCSV.Config Api.Data.Link Effect
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
                    >> RelationDetailsMsg id
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
