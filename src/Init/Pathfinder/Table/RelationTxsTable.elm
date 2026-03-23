module Init.Pathfinder.Table.RelationTxsTable exposing (init)

import Api.Request.Addresses
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Config.Update as Update
import Model.Direction exposing (Direction)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Time
import View.Pathfinder.TransactionFilter as TransactionFilter


init : Update.Config -> ( Time.Posix, Time.Posix ) -> Direction -> List String -> RelationTxsTable.Model
init uc ( mn, mx ) dir assets =
    let
        table isDesc =
            Table.initSorted isDesc RelationTxsTable.titleTimestamp
                |> InfiniteTable.init "relationTxsTable" 25
    in
    { table = table False
    , order = Just Api.Request.Addresses.Order_Desc
    , isTxFilterViewOpen = False
    , exportCSV = ExportCSV.init
    , filter =
        TransactionFilter.init
            |> TransactionFilter.withDateRangePicker uc.locale mn mx
            |> TransactionFilter.withAssetSelectBox assets
            |> TransactionFilter.withDirection (Just dir)
    }
