module Update.Pathfinder.RelationDetails exposing (update)

import Api.Request.Addresses
import Basics.Extra exposing (flip)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Maybe.Extra
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.RelationDetails exposing (Model)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.RelationDetails as RelationDetails exposing (Msg(..))
import PagedTable
import RecordSetter exposing (s_a2bTable, s_a2bTableOpen, s_b2aTable, s_b2aTableOpen, s_table)
import Tuple exposing (first, mapFirst, mapSecond, second)
import Util exposing (n)


loadRelationTxs : ( Id, Id ) -> Bool -> Int -> Maybe String -> Effect
loadRelationTxs id isA2b nrItems nextpage =
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
    in
    BrowserGotLinks isA2b
        >> RelationDetailsMsg id
        |> Api.GetAddresslinkTxsEffect
            { currency = Id.network a
            , source = source
            , target = target
            , minHeight = Nothing
            , maxHeight = Nothing
            , order = Just Api.Request.Addresses.Order_Desc
            , nextpage = nextpage
            , pagesize = nrItems
            }
        |> ApiEffect


tableConfig : ( Id, Id ) -> Bool -> PagedTable.Config Effect
tableConfig id isA2b =
    { fetch = loadRelationTxs id isA2b |> Just
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


update : ( Id, Id ) -> RelationDetails.Msg -> Model -> ( Model, List Effect )
update id msg model =
    case msg of
        UserClickedToggleTable isA2b ->
            let
                gs =
                    gettersAndSetters isA2b

                ( table, eff ) =
                    gs.getTable model
                        |> .table
                        |> PagedTable.loadFirstPage
                            (tableConfig id isA2b)
            in
            ( gs.getOpen model
                |> not
                |> flip gs.setOpen model
                |> gs.setTable (s_table table (gs.getTable model))
            , Maybe.Extra.toList eff
            )

        TableMsg isA2b tm ->
            let
                gs =
                    gettersAndSetters isA2b
            in
            gs.getTable model
                |> .table
                |> PagedTable.update (tableConfig id isA2b) tm
                |> mapFirst (flip s_table (gs.getTable model))
                |> mapFirst (flip gs.setTable model)
                |> mapSecond Maybe.Extra.toList

        BrowserGotLinks isA2b data ->
            let
                gs =
                    gettersAndSetters isA2b
            in
            gs.getTable model
                |> .table
                |> PagedTable.appendData
                    (tableConfig id isA2b)
                    RelationTxsTable.filter
                    data.nextPage
                    data.links
                |> mapFirst (flip s_table (gs.getTable model))
                |> mapFirst (flip gs.setTable model)
                |> mapSecond Maybe.Extra.toList

        RelationDetails.NoOp ->
            n model

        UserClickedAllTxCheckboxInTable ->
            -- handled upstream
            n model

        UserClickedTxCheckboxInTable _ ->
            -- handled upstream
            n model

        RelationDetails.UserClickedTx _ ->
            -- handled upstream
            n model
