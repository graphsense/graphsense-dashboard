module Css.Browser exposing (propertyBoxImage, propertyBoxKey, propertyBoxNote, propertyBoxRoot, propertyBoxRow, propertyBoxRule, propertyBoxTable, propertyBoxTableLink, propertyBoxValueInner)

import Config.View exposing (Config)
import Css exposing (..)


propertyBoxRoot : Config -> List Style
propertyBoxRoot vc =
    vc.theme.browser.propertyBoxRoot vc.lightmode


propertyBoxTable : Config -> List Style
propertyBoxTable vc =
    [ display table
    , borderCollapse collapse
    , position relative
    ]
        ++ vc.theme.browser.propertyBoxTable


propertyBoxNote : Config -> List Style
propertyBoxNote vc =
    vc.theme.browser.propertyBoxNote vc.lightmode


propertyBoxRow : Config -> Bool -> List Style
propertyBoxRow vc active =
    display tableRow
        :: position relative
        :: vc.theme.browser.propertyBoxRow vc.lightmode active


propertyBoxKey : Config -> List Style
propertyBoxKey vc =
    display tableCell
        :: vc.theme.browser.propertyBoxKey


propertyBoxImage : Config -> List Style
propertyBoxImage _ =
    [ display block

    {- :: marginLeft auto
       :: marginRight auto
    -}
    , borderRadius (pct 50)
    , height (px 40)
    , width (px 40)
    ]


propertyBoxValueInner : Config -> List Style
propertyBoxValueInner vc =
    vc.theme.browser.propertyBoxValueInner


propertyBoxRule : Config -> List Style
propertyBoxRule vc =
    vc.theme.browser.propertyBoxRule


propertyBoxTableLink : Config -> Bool -> List Style
propertyBoxTableLink vc active =
    position relative
        :: right (px 0)
        :: paddingLeft (px 5)
        :: borderRight (px 1)
        :: vc.theme.browser.propertyBoxTableLink vc.lightmode active
