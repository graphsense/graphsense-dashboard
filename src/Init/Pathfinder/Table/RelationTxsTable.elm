module Init.Pathfinder.Table.RelationTxsTable exposing (emptyDateFilter, init)

import Api.Request.Addresses
import Init.Graph.Table
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Msg.Pathfinder.RelationDetails exposing (Msg)
import PagedTable
import Util.ThemedSelectBox as ThemedSelectBox


itemsPerPage : Int
itemsPerPage =
    5


emptyDateFilter : { txMinBlock : Maybe Int, txMaxBlock : Maybe Int, dateRangePicker : Maybe (DateRangePicker.Model Msg) }
emptyDateFilter =
    { txMinBlock = Nothing, txMaxBlock = Nothing, dateRangePicker = Nothing }


getCompleteAssetList : List String -> List (Maybe String)
getCompleteAssetList l =
    Nothing :: (l |> List.map Just)


init : Direction -> List String -> RelationTxsTable.Model
init dir assets =
    let
        table isDesc =
            Init.Graph.Table.initSorted isDesc RelationTxsTable.titleTimestamp
                |> PagedTable.init
                |> PagedTable.setNrItems itemsPerPage
                |> PagedTable.setItemsPerPage itemsPerPage
    in
    { table = table False
    , order = Just Api.Request.Addresses.Order_Desc
    , dateRangePicker = Nothing
    , txMinBlock = Nothing
    , txMaxBlock = Nothing
    , direction = Just dir
    , isTxFilterViewOpen = False
    , assetSelectBox = ThemedSelectBox.init (getCompleteAssetList assets)
    , selectedAsset = Nothing
    }



-- initWithFilter : (Id, Id) -> Bool -> { x | txMinBlock : Maybe Int, txMaxBlock : Maybe Int, dateRangePicker : Maybe (DateRangePicker.Model Msg) } -> Maybe String -> List String -> (RelationTxsTable.Model, List Effect)
-- initWithFilter rel isA2b dateFilter selectedAsset assets =
--     let
--         a =
--             first rel
--         b =
--             second rel
--         ( source, target ) =
--             if isA2b then
--                 ( Id.id a, Id.id b )
--             else
--                 ( Id.id b, Id.id a )
--         table isDesc =
--             Init.Graph.Table.initSorted isDesc RelationTxsTable.titleTimestamp
--                 |> PagedTable.init
--                 -- |> PagedTable.setNrItems itemsPerPage
--                 |> PagedTable.setItemsPerPage itemsPerPage
--     in
--     ({ table = table False
--     , order = Just Api.Request.Addresses.Order_Desc
--     , dateRangePicker = dateFilter.dateRangePicker
--     , txMinBlock = dateFilter.txMinBlock
--     , txMaxBlock = dateFilter.txMaxBlock
--     , isTxFilterViewOpen = False
--     , assetSelectBox = ThemedSelectBox.init (getCompleteAssetList assets)
--     , selectedAsset = selectedAsset
--     }, [
--         (BrowserGotLinks isA2b >> RelationDetailsMsg rel)
--         |> Api.GetAddresslinkTxsEffect
--             { currency = Id.network a
--             , source =  source
--             , target = target
--             , pagesize = itemsPerPage
--             , nextpage = Nothing
--             , order = Nothing
--             , tokenCurrency = selectedAsset
--             , minHeight = dateFilter.txMinBlock
--             , maxHeight = dateFilter.txMaxBlock
--             }
--         |> ApiEffect
--     ] )
