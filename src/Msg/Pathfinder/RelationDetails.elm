module Msg.Pathfinder.RelationDetails exposing (Msg(..))

import Api.Data
import Components.ExportCSV as ExportCSV
import Components.InfiniteTable as InfiniteTable
import DurationDatePicker
import Model.Pathfinder.Id exposing (Id)
import Util.ThemedSelectBox as ThemedSelectBox


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
    | OpenDateRangePicker Bool
    | CloseDateRangePicker Bool
    | ResetDateRangePicker Bool
    | UpdateDateRangePicker Bool DurationDatePicker.Msg
    | ResetAllTxFilters Bool
    | ResetTxAssetFilter Bool
    | TxTableAssetSelectBoxMsg Bool (ThemedSelectBox.Msg (Maybe String))
    | ExportCSVMsg Bool ExportCSV.Msg
    | BrowserGotLinksForExport Bool (Maybe String) Api.Data.Links
