module Css.Table exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> List Style
root vc =
    vc.theme.table.root


tableRoot : Config -> List Style
tableRoot vc =
    overflowY auto
        :: overflowX auto
        :: vc.theme.table.tableRoot


sidebar : Config -> List Style
sidebar vc =
    vc.theme.table.sidebar vc.lightmode


filter : Config -> List Style
filter vc =
    vc.theme.table.filter


filterInput : Config -> List Style
filterInput vc =
    vc.theme.table.filterInput vc.lightmode


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
    vc.theme.table.row vc.lightmode


cell : Config -> List Style
cell vc =
    vc.theme.table.cell


valuesCell : Config -> Bool -> List Style
valuesCell vc isNegative =
    vc.theme.table.valuesCell vc.lightmode isNegative


numberCell : Config -> List Style
numberCell vc =
    vc.theme.table.numberCell


loadingSpinner : Config -> List Style
loadingSpinner vc =
    vc.theme.table.loadingSpinner


emptyHint : Config -> List Style
emptyHint vc =
    vc.theme.table.emptyHint


tick : Config -> List Style
tick vc =
    vc.theme.table.tick
