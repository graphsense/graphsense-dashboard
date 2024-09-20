module Css.Table exposing (Styles, loadingSpinner, row, styles, table)

import Config.View as View exposing (Config)
import Css exposing (..)


root : Config -> List Style
root vc =
    displayFlex
        :: flexDirection Css.row
        :: overflowX auto
        :: position relative
        :: vc.theme.table.root


tableRoot : Config -> List Style
tableRoot vc =
    overflowY auto
        :: overflowX auto
        :: displayFlex
        :: flexDirection column
        :: maxHeight (px vc.theme.table.maxHeight)
        :: vc.theme.table.tableRoot


sidebar : Config -> List Style
sidebar vc =
    vc.theme.table.sidebar vc.lightmode


sidebarIcon : Config -> Bool -> List Style
sidebarIcon vc =
    vc.theme.table.sidebarIcon vc.lightmode


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
    vc.theme.table.headCell vc.lightmode


headRow : Config -> List Style
headRow vc =
    vc.theme.table.headRow


headCellSortable : Config -> List Style
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
    display inlineBlock :: vc.theme.table.tick


info : Config -> List Style
info vc =
    position absolute
        :: bottom zero
        :: left zero
        :: vc.theme.table.info vc.lightmode


type alias Styles =
    { root : View.Config -> List Style
    , tableRoot : View.Config -> List Style
    , filter : View.Config -> List Style
    , filterInput : View.Config -> List Style
    , loadingSpinner : View.Config -> List Style
    , sidebar : View.Config -> List Style
    , sidebarIcon : View.Config -> Bool -> List Style
    , table : View.Config -> List Style
    , row : View.Config -> List Style
    , headRow : View.Config -> List Style
    , cell : View.Config -> List Style
    , headCellSortable : View.Config -> List Style
    , headCell : View.Config -> List Style
    , numberCell : View.Config -> List Style
    , valuesCell : View.Config -> Bool -> List Style
    , tick : View.Config -> List Style
    , info : View.Config -> List Style
    , emptyHint : View.Config -> List Style
    }


styles : Styles
styles =
    { root = root
    , tableRoot = tableRoot
    , filter = filter
    , filterInput = filterInput
    , loadingSpinner = loadingSpinner
    , sidebar = sidebar
    , sidebarIcon = sidebarIcon
    , table = table
    , row = row
    , headRow = headRow
    , cell = cell
    , headCellSortable = headCellSortable
    , headCell = headCell
    , numberCell = numberCell
    , valuesCell = valuesCell
    , tick = tick
    , info = info
    , emptyHint = emptyHint
    }
