module Css.Stats exposing (currency, currencyBackground, currencyBackgroundPath, currencyHeading, loadingSpinner, root, stats, statsBadge, statsBadgeContainer, statsTable, statsTableCellKey, statsTableCellValue, statsTableInnerWrapper, statsTableRow, statsTableWrapper)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> List Style
root vc =
    vc.theme.stats.root


stats : Config -> List Style
stats vc =
    [ displayFlex
    , flexWrap wrap
    ]
        ++ vc.theme.stats.stats


currency : Config -> List Style
currency vc =
    vc.theme.stats.currency vc.lightmode


currencyHeading : Config -> List Style
currencyHeading vc =
    vc.theme.stats.currencyHeading vc.lightmode


statsTableWrapper : Config -> List Style
statsTableWrapper vc =
    position relative :: vc.theme.stats.tableWrapper


statsTableInnerWrapper : Config -> List Style
statsTableInnerWrapper vc =
    [ zIndex <| int 20
    , position relative
    ]
        ++ vc.theme.stats.tableWrapperInner


statsTable : Config -> List Style
statsTable vc =
    display table
        :: vc.theme.stats.table


statsTableRow : Config -> List Style
statsTableRow vc =
    display tableRow
        :: vc.theme.stats.tableRow


statsTableCellValue : Config -> List Style
statsTableCellValue vc =
    display tableCell
        :: vc.theme.stats.tableCellValue


statsTableCellKey : Config -> List Style
statsTableCellKey vc =
    display tableCell
        :: vc.theme.stats.tableCellKey


statsBadge : Config -> List Style
statsBadge vc =
    [ px 50 |> width
    , textAlign center
    ]
        ++ vc.theme.stats.tokenBadgeStyle vc.lightmode


statsBadgeContainer : List Style
statsBadgeContainer =
    [ Css.displayFlex
    , Css.flexDirection Css.row
    , Css.flexWrap Css.wrap
    , Css.property "align-content" "space-between"
    , Css.property "gap" "2px"
    , Css.width (Css.px 130)
    ]


currencyBackground : Config -> List Style
currencyBackground vc =
    [ position absolute
    , displayFlex
    , pct 100 |> width
    , pct 100 |> height
    , zIndex <| int 10
    , px 0 |> top
    , justifyContent center
    , alignItems stretch
    ]
        ++ vc.theme.stats.currencyBackground


currencyBackgroundPath : Config -> List Style
currencyBackgroundPath vc =
    fill currentColor
        :: vc.theme.stats.currencyBackgroundPath


loadingSpinner : Config -> List Style
loadingSpinner vc =
    vc.theme.stats.loadingSpinner
