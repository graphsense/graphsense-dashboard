module Update.Pathfinder.Table.TransactionTable exposing (sort, updateQuickFilters)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.TransactionFilter as TransactionFilter
import Model.Pathfinder.Table.TransactionTable as TransactionTable


updateQuickFilters : List TransactionFilter.QuickFilter -> TransactionTable.Model -> TransactionTable.Model
updateQuickFilters quickFilters tableModel =
    { tableModel
        | filter =
            TransactionFilter.updateQuickFilters quickFilters tableModel.filter
    }


sort : Bool -> InfiniteTable.Model Api.Data.AddressTx -> InfiniteTable.Model Api.Data.AddressTx
sort =
    InfiniteTable.sortBy TransactionTable.titleTimestamp
