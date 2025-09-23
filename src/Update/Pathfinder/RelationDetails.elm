module Update.Pathfinder.RelationDetails exposing (gettersAndSetters, update)

import Api.Request.Addresses
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Config.DateRangePicker exposing (datePickerSettings)
import Config.Update as Update
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.DateRangePicker as DateRangePicker
import Init.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Maybe.Extra
import Model.Direction exposing (Direction(..))
import Model.Locale as Locale
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.RelationDetails exposing (Model)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Model.Pathfinder.Tx exposing (getRawTimestampForRelationTx)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.RelationDetails as RelationDetails exposing (Msg(..))
import RecordSetter exposing (s_a2bTable, s_a2bTableOpen, s_assetSelectBox, s_b2aTable, s_b2aTableOpen, s_dateRangePicker, s_isTxFilterViewOpen, s_selectedAsset, s_table)
import Time
import Tuple exposing (first, mapFirst, mapSecond, second)
import Update.DateRangePicker as DateRangePicker
import Util exposing (n)
import Util.ThemedSelectBox as ThemedSelectBox


loadRelationTxs : ( Id, Id ) -> Bool -> RelationTxsTable.Model -> Maybe ( String, Bool ) -> Int -> Maybe String -> Effect
loadRelationTxs id isA2b txTable sorting nrItems nextpage =
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

        msg =
            if nextpage == Nothing then
                BrowserGotLinks

            else
                BrowserGotLinksNextPage

        fromD =
            txTable.dateRangePicker |> Maybe.andThen .fromDate

        toD =
            txTable.dateRangePicker |> Maybe.andThen .toDate
    in
    msg isA2b
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


tableConfig : ( Id, Id ) -> Bool -> RelationTxsTable.Model -> InfiniteTable.Config Effect
tableConfig id isA2b txTable =
    { fetch = loadRelationTxs id isA2b txTable
    , triggerOffset = 100
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


update : Update.Config -> ( Id, Id ) -> ( Time.Posix, Time.Posix ) -> RelationDetails.Msg -> Model -> ( Model, List Effect )
update uc id ( rangeFrom, rangeTo ) msg model =
    let
        net =
            id |> Tuple.first |> Id.network

        dir isA2b =
            if isA2b then
                Incoming

            else
                Outgoing
    in
    case msg of
        UserClickedToggleTable isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model

                isOpen =
                    gs.getOpen model

                ( table, eff ) =
                    if isOpen then
                        ( tbl.table, Nothing )

                    else
                        tbl.table
                            |> InfiniteTable.loadFirstPage
                                (tableConfig id isA2b tbl)
            in
            ( isOpen
                |> not
                |> flip gs.setOpen model
                |> gs.setTable (s_table table tbl)
            , Maybe.Extra.toList eff
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
                |> mapSecond Maybe.Extra.toList
                |> mapSecond
                    ((::)
                        (cmd
                            |> Cmd.map (TableMsg isA2b >> RelationDetailsMsg id)
                            |> CmdEffect
                        )
                    )

        BrowserGotLinks isA2b data ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model

                ( table, cmd, eff ) =
                    tbl
                        |> .table
                        |> InfiniteTable.reset
                        |> InfiniteTable.appendData
                            (tableConfig id isA2b tbl)
                            RelationTxsTable.filter
                            data.nextPage
                            data.links
            in
            ( table, eff )
                |> mapFirst (flip s_table tbl)
                |> mapFirst (flip gs.setTable model)
                |> mapSecond Maybe.Extra.toList
                |> mapSecond
                    ((::)
                        (cmd
                            |> Cmd.map (TableMsg isA2b >> RelationDetailsMsg id)
                            |> CmdEffect
                        )
                    )

        BrowserGotLinksNextPage isA2b data ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model

                ( table, cmd, eff ) =
                    tbl
                        |> .table
                        |> InfiniteTable.appendData
                            (tableConfig id isA2b tbl)
                            RelationTxsTable.filter
                            data.nextPage
                            data.links
            in
            ( table, eff )
                |> mapFirst (flip s_table tbl)
                |> mapFirst (flip gs.setTable model)
                |> mapSecond Maybe.Extra.toList
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
                                    ( udateTbl.table, Nothing )
                        in
                        ( model |> gs.setTable (udateTbl |> s_table ntbl)
                        , if dateRangeChanged then
                            eff

                          else
                            Nothing
                        )
                            |> mapSecond Maybe.Extra.toList
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
                    RelationTxsTable.init (dir isA2b) (Locale.getTokenTickers uc.locale net)
                        |> s_selectedAsset oldTable.selectedAsset
                        |> s_assetSelectBox oldTable.assetSelectBox

                ( table, eff ) =
                    tbl
                        |> .table
                        |> InfiniteTable.loadFirstPage
                            (tableConfig id isA2b tbl)
                        |> mapSecond Maybe.Extra.toList
            in
            ( model |> gs.setTable (tbl |> s_table table)
            , eff
            )

        ResetAllTxFilters isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    RelationTxsTable.init (dir isA2b) (Locale.getTokenTickers uc.locale net)

                ( table, eff ) =
                    tbl
                        |> .table
                        |> InfiniteTable.loadFirstPage
                            (tableConfig id isA2b tbl)
                        |> mapSecond Maybe.Extra.toList
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
                    RelationTxsTable.init (dir isA2b) (Locale.getTokenTickers uc.locale net)
                        |> s_dateRangePicker oldTable.dateRangePicker

                ( table, eff ) =
                    tbl
                        |> .table
                        |> InfiniteTable.loadFirstPage
                            (tableConfig id isA2b tbl)
                        |> mapSecond Maybe.Extra.toList
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
            if tbl == newTxs then
                n model

            else
                let
                    ( ntbl, eff ) =
                        newTxs
                            |> .table
                            |> InfiniteTable.loadFirstPage
                                (tableConfig id isA2b newTxs)
                in
                ( model |> gs.setTable (newTxs |> s_table ntbl), eff ) |> mapSecond Maybe.Extra.toList
