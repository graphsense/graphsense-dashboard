module Init.Pathfinder.Table.RelationTxsTable exposing (emptyDateFilter, init)

import Api.Request.Addresses
import Init.Graph.Table
import Model.DateRangePicker as DateRangePicker
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


init : List String -> RelationTxsTable.Model
init assets =
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
    , isTxFilterViewOpen = False
    , assetSelectBox = ThemedSelectBox.init (getCompleteAssetList assets)
    , selectedAsset = Nothing
    }
