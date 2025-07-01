module Update.Pathfinder.RelationDetails exposing (gettersAndSetters, update)

import Api.Request.Addresses
import Basics.Extra exposing (flip)
import Config.DateRangePicker exposing (datePickerSettingsWithoutRange)
import Config.Update as Update
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.DateRangePicker as DateRangePicker
import Maybe.Extra
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.RelationDetails exposing (Model)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.RelationDetails as RelationDetails exposing (Msg(..))
import PagedTable
import RecordSetter exposing (s_a2bTable, s_a2bTableOpen, s_b2aTable, s_b2aTableOpen, s_dateRangePicker, s_isTxFilterViewOpen, s_table)
import Time
import Tuple exposing (first, mapFirst, mapSecond, second)
import Update.DateRangePicker as DateRangePicker
import Util exposing (n)


loadRelationTxs : ( Id, Id ) -> Bool -> RelationTxsTable.Model -> Int -> Maybe String -> Effect
loadRelationTxs id isA2b txTable nrItems nextpage =
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
    in
    msg isA2b
        >> RelationDetailsMsg id
        |> Api.GetAddresslinkTxsEffect
            { currency = Id.network a
            , source = source
            , target = target
            , minHeight = txTable.txMinBlock
            , maxHeight = txTable.txMaxBlock
            , tokenCurrency = txTable.selectedAsset
            , order = Just Api.Request.Addresses.Order_Desc
            , nextpage = nextpage
            , pagesize = nrItems
            }
        |> ApiEffect


tableConfig : ( Id, Id ) -> Bool -> RelationTxsTable.Model -> PagedTable.Config Effect
tableConfig id isA2b txTable =
    { fetch = loadRelationTxs id isA2b txTable |> Just
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
    case msg of
        UserClickedToggleTable isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model

                ( table, eff ) =
                    tbl
                        |> .table
                        |> PagedTable.loadFirstPage
                            (tableConfig id isA2b tbl)
            in
            ( gs.getOpen model
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
            in
            tbl
                |> .table
                |> PagedTable.update (tableConfig id isA2b tbl) tm
                |> mapFirst (flip s_table tbl)
                |> mapFirst (flip gs.setTable model)
                |> mapSecond Maybe.Extra.toList

        BrowserGotLinks isA2b data ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model
            in
            tbl
                |> .table
                |> PagedTable.setData
                    (tableConfig id isA2b tbl)
                    RelationTxsTable.filter
                    data.nextPage
                    data.links
                |> mapFirst (flip s_table tbl)
                |> mapFirst (flip gs.setTable model)
                |> mapSecond Maybe.Extra.toList

        BrowserGotLinksNextPage isA2b data ->
            let
                gs =
                    gettersAndSetters isA2b

                tbl =
                    gs.getTable model
            in
            tbl
                |> .table
                |> PagedTable.appendData
                    (tableConfig id isA2b tbl)
                    RelationTxsTable.filter
                    data.nextPage
                    data.links
                |> mapFirst (flip s_table tbl)
                |> mapFirst (flip gs.setTable model)
                |> mapSecond Maybe.Extra.toList

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
            in
            tbl.dateRangePicker
                |> Maybe.withDefault
                    (datePickerSettingsWithoutRange uc.locale
                        |> DateRangePicker.init (UpdateDateRangePicker isA2b) rangeFrom rangeTo
                    )
                |> DateRangePicker.openPicker
                |> Just
                |> flip s_dateRangePicker tbl
                |> flip gs.setTable model
                |> n

        UpdateDateRangePicker isA2b subMsg ->
            n model

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
            n model

        ResetAllTxFilters isA2b ->
            n model

        ResetTxAssetFilter isA2b ->
            n model

        TxTableAssetSelectBoxMsg isA2b _ ->
            n model
