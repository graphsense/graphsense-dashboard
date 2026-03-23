module Msg.Pathfinder.RelationDetails exposing (Msg(..))

import Api.Data
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import Components.TransactionFilter as TransactionFilter


type Msg
    = UserClickedToggleTable Bool
    | TableMsg Bool InfiniteTable.Msg
    | BrowserGotLinks Bool (Maybe String) Api.Data.Links
    | UserClickedAllTxCheckboxInTable Bool
    | UserClickedTxCheckboxInTable Api.Data.Link
    | UserClickedTx Id
    | NoOp
    | ToggleTxFilterView Bool
    | CloseTxFilterView Bool
    | TransactionFilterMsg Bool TransactionFilter.Msg
    | ExportCSVMsg Bool RelationTxsTable.Model ExportCSV.Msg
    | BrowserGotLinksForExport Bool RelationTxsTable.Model Api.Data.Links
