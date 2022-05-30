module Css.Table exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> List Style
root vc =
    overflowY auto
        :: overflowX auto
        :: vc.theme.table.root


table : Config -> List Style
table vc =
    vc.theme.table.table


headCell : Config -> List Style
headCell vc =
    vc.theme.table.headCell


headRow : Config -> List Style
headRow vc =
    vc.theme.table.headRow


headCellSortable : Config -> List ( String, String )
headCellSortable vc =
    vc.theme.table.headCellSortable


row : Config -> List Style
row vc =
    vc.theme.table.row


cell : Config -> List Style
cell vc =
    vc.theme.table.cell


valuesCell : Config -> Bool -> List Style
valuesCell vc isNegative =
    vc.theme.table.valuesCell isNegative


numberCell : Config -> List Style
numberCell vc =
    vc.theme.table.numberCell


loadingSpinner : Config -> List Style
loadingSpinner vc =
    vc.theme.table.loadingSpinner
