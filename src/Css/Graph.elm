module Css.Graph exposing (..)

import Color
import Config.Graph exposing (addressesCountHeight, labelHeight, linkLabelHeight)
import Config.View exposing (Config)
import Css exposing (..)
import Model.Graph exposing (NodeType)
import Model.Graph.Tool as Tool
import Util.Css


contextMenuRule : Config -> List Style
contextMenuRule vc =
    vc.theme.graph.contextMenuRule vc.lightmode


navbar : Config -> List Style
navbar vc =
    [ displayFlex
    , flexDirection row
    ]
        ++ vc.theme.graph.navbar vc.lightmode


navbarLeft : Config -> List Style
navbarLeft vc =
    [ displayFlex
    , flexDirection row
    , alignItems center
    , justifyContent flexStart
    ]
        ++ vc.theme.graph.navbarLeft


navbarRight : Config -> List Style
navbarRight vc =
    [ displayFlex
    , flexDirection row
    , alignItems center
    , flexGrow <| num 1
    , justifyContent flexEnd
    ]
        ++ vc.theme.graph.navbarRight


tool : Config -> Tool.Status -> List Style
tool vc status =
    cursor pointer
        :: vc.theme.graph.tool vc.lightmode status


svgRoot : Config -> List Style
svgRoot vc =
    [ pct 100 |> width
    , property "color" "black"
    ]
        ++ vc.theme.graph.svgRoot vc.lightmode


addressRoot : Config -> Bool -> List Style
addressRoot vc highlighter =
    cursor
        (if highlighter then
            crosshair

         else
            pointer
        )
        :: vc.theme.graph.addressRoot


addressRect : Config -> List Style
addressRect vc =
    vc.theme.graph.addressRect


nodeFrame : Config -> NodeType -> Bool -> List Style
nodeFrame vc nodeType isSelected =
    [ property "stroke" "currentColor"
    , property "stroke-width" "1px"
    ]
        ++ vc.theme.graph.nodeFrame vc.lightmode nodeType isSelected


addressFlags : Config -> List Style
addressFlags vc =
    vc.theme.graph.addressFlags


abuseFlag : Config -> List Style
abuseFlag vc =
    vc.theme.graph.abuseFlag vc.lightmode


flag : Config -> List Style
flag vc =
    vc.theme.graph.flag vc.lightmode


addressLabel : Config -> List Style
addressLabel vc =
    property "fill" "currentColor"
        :: (px labelHeight |> fontSize)
        :: vc.theme.graph.addressLabel vc.lightmode


entityRoot : Config -> Bool -> List Style
entityRoot vc highlighter =
    cursor
        (if highlighter then
            crosshair

         else
            pointer
        )
        :: vc.theme.graph.entityRoot


entityRect : Config -> List Style
entityRect vc =
    vc.theme.graph.entityRect


frameStyle : List Style
frameStyle =
    [ property "stroke" "currentColor"
    , property "stroke-width" "1px"
    ]


entityFlags : Config -> List Style
entityFlags vc =
    vc.theme.graph.entityFlags


entityLabel : Config -> List Style
entityLabel vc =
    property "fill" "currentColor"
        :: (px labelHeight |> fontSize)
        :: vc.theme.graph.entityLabel vc.lightmode


labelText : Config -> NodeType -> List Style
labelText vc nodeType =
    vc.theme.graph.labelText nodeType


shadowLink : Config -> List Style
shadowLink vc =
    vc.theme.graph.shadowLink vc.lightmode


entityCurrency : Config -> List Style
entityCurrency vc =
    property "fill" "currentColor"
        :: vc.theme.graph.entityCurrency vc.lightmode


entityAddressesCount : Config -> List Style
entityAddressesCount vc =
    property "fill" "currentColor"
        :: (px addressesCountHeight |> fontSize)
        :: vc.theme.graph.entityAddressesCount vc.lightmode


expandHandle : Config -> NodeType -> List Style
expandHandle vc nodeType =
    cursor pointer
        :: vc.theme.graph.expandHandle nodeType


expandHandlePath : Config -> NodeType -> Bool -> List Style
expandHandlePath vc nodeType isSelected =
    frameStyle
        ++ vc.theme.graph.expandHandlePath vc.lightmode nodeType isSelected


expandHandleText : Config -> NodeType -> List Style
expandHandleText vc nodeType =
    property "fill" "currentColor"
        :: vc.theme.graph.expandHandleText vc.lightmode nodeType


nodeSeparatorToExpandHandle : Config -> NodeType -> List Style
nodeSeparatorToExpandHandle vc nodeType =
    frameStyle
        ++ vc.theme.graph.nodeSeparatorToExpandHandle vc.lightmode nodeType


link : Config -> NodeType -> Bool -> Bool -> Maybe Color.Color -> List Style
link vc nodeType hovered selected highlight =
    [ property "stroke" "currentColor"
    , property "fill" "none"
    ]
        ++ vc.theme.graph.link vc.lightmode nodeType hovered selected highlight


linkLabel : Config -> Bool -> Bool -> Maybe Color.Color -> List Style
linkLabel vc hovered selected color =
    (px linkLabelHeight |> fontSize)
        :: property "fill" "currentColor"
        :: vc.theme.graph.linkLabel vc.lightmode hovered selected color


linkLabelBox : Config -> Bool -> Bool -> List Style
linkLabelBox vc hovered selected =
    vc.theme.graph.linkLabelBox vc.lightmode hovered selected


searchTextarea : Config -> List Style
searchTextarea vc =
    vc.theme.graph.searchTextarea vc.lightmode


toolbox : Config -> Bool -> List Style
toolbox vc visible =
    position absolute
        :: Util.Css.zIndexMain
        :: vc.theme.graph.toolbox vc.lightmode visible


legendItem : Config -> List Style
legendItem vc =
    vc.theme.graph.legendItem


legendItemColor : Config -> List Style
legendItemColor vc =
    vc.theme.graph.legendItemColor


legendItemTitle : Config -> List Style
legendItemTitle vc =
    vc.theme.graph.legendItemTitle


radio : Config -> List Style
radio vc =
    vc.theme.graph.radio


radioInput : Config -> List Style
radioInput vc =
    vc.theme.graph.radioInput


radioText : Config -> List Style
radioText vc =
    vc.theme.graph.radioText


searchSettingsRow : Config -> List Style
searchSettingsRow vc =
    vc.theme.graph.searchSettingsRow


tagLockedIcon : Config -> List Style
tagLockedIcon vc =
    vc.theme.graph.tagLockedIcon


tagLockedText : Config -> List Style
tagLockedText vc =
    vc.theme.graph.tagLockedText


highlightsRoot : Config -> List Style
highlightsRoot vc =
    vc.theme.graph.highlightsRoot


highlights : Config -> List Style
highlights vc =
    vc.theme.graph.highlights


highlightsColors : Config -> List Style
highlightsColors vc =
    vc.theme.graph.highlightsColors


highlightsColor : Config -> List Style
highlightsColor vc =
    vc.theme.graph.highlightsColor


highlightRoot : Config -> List Style
highlightRoot vc =
    vc.theme.graph.highlightRoot


highlightColor : Config -> Bool -> List Style
highlightColor vc selected =
    cursor pointer
        :: vc.theme.graph.highlightColor vc.lightmode selected


highlightTitle : Config -> List Style
highlightTitle vc =
    vc.theme.graph.highlightTitle vc.lightmode


highlightTrash : Config -> List Style
highlightTrash vc =
    vc.theme.graph.highlightTrash vc.lightmode
