module Css.Browser exposing (..)

import Config.View exposing (Config)
import Css exposing (..)


root : Config -> List Style
root vc =
    position absolute
        :: height (px 0)
        :: (zIndex <| int 50)
        :: vc.theme.browser.root


frame : Config -> Bool -> List Style
frame vc visible =
    backgroundColor (rgb 255 255 255)
        :: (if visible then
                displayFlex

            else
                display none
           )
        :: vc.theme.browser.frame vc.lightmode visible


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


propertyBoxRow : Config -> List Style
propertyBoxRow vc =
    display tableRow
        :: position relative
        :: vc.theme.browser.propertyBoxRow vc.lightmode


propertyBoxKey : Config -> List Style
propertyBoxKey vc =
    display tableCell
        :: vc.theme.browser.propertyBoxKey


propertyBoxValue : Config -> List Style
propertyBoxValue vc =
    display tableCell
        :: vc.theme.browser.propertyBoxValue


propertyBoxValueInner : Config -> List Style
propertyBoxValueInner vc =
    vc.theme.browser.propertyBoxValueInner


propertyBoxEntityId : Config -> List Style
propertyBoxEntityId vc =
    vc.theme.browser.propertyBoxEntityId vc.lightmode


propertyBoxRule : Config -> List Style
propertyBoxRule vc =
    vc.theme.browser.propertyBoxRule


propertyBoxIncomingTxs : Config -> List Style
propertyBoxIncomingTxs vc =
    vc.theme.browser.propertyBoxIncomingTxs vc.lightmode


propertyBoxOutgoingTxs : Config -> List Style
propertyBoxOutgoingTxs vc =
    vc.theme.browser.propertyBoxOutgoingTxs vc.lightmode


propertyBoxUsageTimestamp : Config -> List Style
propertyBoxUsageTimestamp vc =
    vc.theme.browser.propertyBoxUsageTimestamp


propertyBoxUsageRelative : Config -> List Style
propertyBoxUsageRelative vc =
    vc.theme.browser.propertyBoxUsageRelative


propertyBoxActivityPeriod : Config -> List Style
propertyBoxActivityPeriod vc =
    vc.theme.browser.propertyBoxActivityPeriod


propertyBoxTableLink : Config -> Bool -> List Style
propertyBoxTableLink vc active =
    position absolute
        :: right (px 0)
        :: vc.theme.browser.propertyBoxTableLink vc.lightmode active


loadingSpinner : Config -> List Style
loadingSpinner vc =
    vc.theme.browser.loadingSpinner