module Update.Pathfinder.Table.TransactionTable exposing (updateQuickFilters)

import Components.TransactionFilter as TransactionFilter
import Model.Pathfinder.Table.TransactionTable as TransactionTable


updateQuickFilters : List TransactionFilter.QuickFilter -> TransactionTable.Model -> TransactionTable.Model
updateQuickFilters quickFilters tableModel =
    { tableModel
        | filter =
            TransactionFilter.updateQuickFilters quickFilters tableModel.filter
    }
