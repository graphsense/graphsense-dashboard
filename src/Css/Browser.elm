module Css.Browser exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> List Style
root vc =
    position absolute
        :: vc.theme.browser.root


frame : Config -> Bool -> List Style
frame vc visible =
    backgroundColor (rgb 255 255 255)
        :: (if visible then
                display block

            else
                display none
           )
        :: vc.theme.browser.frame visible


propertyBoxTable : Config -> List Style
propertyBoxTable vc =
    [ display table
    , borderCollapse collapse
    , pct 100 |> width
    ]
        ++ vc.theme.browser.propertyBoxTable


propertyBoxRow : Config -> List Style
propertyBoxRow vc =
    display tableRow
        :: vc.theme.browser.propertyBoxRow


propertyBoxKey : Config -> List Style
propertyBoxKey vc =
    display tableCell
        :: vc.theme.browser.propertyBoxKey


propertyBoxValue : Config -> List Style
propertyBoxValue vc =
    display tableCell
        :: vc.theme.browser.propertyBoxValue


propertyBoxEntityId : Config -> List Style
propertyBoxEntityId vc =
    vc.theme.browser.propertyBoxEntityId
