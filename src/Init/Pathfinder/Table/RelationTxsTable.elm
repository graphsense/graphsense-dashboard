module Init.Pathfinder.Table.RelationTxsTable exposing (init)

import Api.Request.Addresses
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Components.TransactionFilter as TransactionFilter
import Config.Update as Update
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Time


init : Update.Config -> Maybe TransactionFilter.Settings -> ( Time.Posix, Time.Posix ) -> Maybe (List String) -> RelationTxsTable.Model
init uc txsFilter ( mn, mx ) assets =
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
        txsFilter
            |> Maybe.withDefault TransactionFilter.initSettings
            |> TransactionFilter.init
            |> TransactionFilter.withDateRangePicker uc.locale mn mx
            |> (assets
                    |> Maybe.map TransactionFilter.withAssetSelectBox
                    |> Maybe.withDefault identity
               )
    }
