module Init.Pathfinder.Table.RelationTxsTable exposing (emptyDateFilter, init)

import Api.Request.Addresses
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Msg.Pathfinder.RelationDetails exposing (Msg)
import Util.ThemedSelectBox as ThemedSelectBox


emptyDateFilter : { txMinBlock : Maybe Int, txMaxBlock : Maybe Int, dateRangePicker : Maybe (DateRangePicker.Model Msg) }
emptyDateFilter =
    { txMinBlock = Nothing, txMaxBlock = Nothing, dateRangePicker = Nothing }


getCompleteAssetList : List String -> List (Maybe String)
getCompleteAssetList l =
    Nothing :: (l |> List.map Just)


init : Direction -> List String -> RelationTxsTable.Model Msg
init dir assets =
    let
        table isDesc =
            Table.initSorted isDesc RelationTxsTable.titleTimestamp
                |> InfiniteTable.init "relationTxsTable" 25
    in
    { table = table False
    , order = Just Api.Request.Addresses.Order_Desc
    , dateRangePicker = Nothing
    , direction = Just dir
    , isTxFilterViewOpen = False
    , assetSelectBox = ThemedSelectBox.init (getCompleteAssetList assets)
    , selectedAsset = Nothing
    , includeZeroValueTxs = Nothing -- Backend does not support this filter at the moment
    , exportCSV = ExportCSV.init
    }
