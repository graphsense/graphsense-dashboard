module Css.Graph exposing (..)

import Color
import Config.Graph exposing (addressesCountHeight, labelHeight, linkLabelHeight)
import Config.View exposing (Config)
import Css exposing (..)
import Model.Graph exposing (NodeType)
import Model.Graph.Tool as Tool


root : Config -> List Style
root vc =
    [ displayFlex
    , flexDirection column
    , pct 100 |> height
    ]
        ++ vc.theme.graph.root


navbar : Config -> List Style
navbar vc =
    [ zIndex <| int 50
    , displayFlex
    , flexDirection row
    ]
        ++ vc.theme.graph.navbar


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
        :: vc.theme.graph.tool status


graphRoot : Config -> List Style
graphRoot vc =
    [ overflow hidden
    , position relative
    , pct 100 |> height
    ]
        ++ vc.theme.graph.graphRoot


svgRoot : Config -> List Style
svgRoot vc =
    [ pct 100 |> width
    , property "color" "black"
    ]
        ++ vc.theme.graph.svgRoot


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
        ++ vc.theme.graph.nodeFrame nodeType isSelected


addressFlags : Config -> List Style
addressFlags vc =
    vc.theme.graph.addressFlags


abuseFlag : Config -> List Style
abuseFlag vc =
    vc.theme.graph.abuseFlag


tagsFlag : Config -> List Style
tagsFlag vc =
    vc.theme.graph.tagsFlag


addressLabel : Config -> List Style
addressLabel vc =
    property "fill" "currentColor"
        :: (px labelHeight |> fontSize)
        :: vc.theme.graph.addressLabel


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
        :: vc.theme.graph.entityLabel


labelText : Config -> NodeType -> List Style
labelText vc nodeType =
    vc.theme.graph.labelText nodeType


shadowLink : Config -> List Style
shadowLink vc =
    vc.theme.graph.shadowLink


entityCurrency : Config -> List Style
entityCurrency vc =
    property "fill" "currentColor"
        :: vc.theme.graph.entityCurrency


entityAddressesCount : Config -> List Style
entityAddressesCount vc =
    property "fill" "currentColor"
        :: (px addressesCountHeight |> fontSize)
        :: vc.theme.graph.entityAddressesCount


expandHandle : Config -> NodeType -> List Style
expandHandle vc nodeType =
    cursor pointer
        :: vc.theme.graph.expandHandle nodeType


expandHandlePath : Config -> NodeType -> Bool -> List Style
expandHandlePath vc nodeType isSelected =
    frameStyle
        ++ vc.theme.graph.expandHandlePath nodeType isSelected


expandHandleText : Config -> NodeType -> List Style
expandHandleText vc nodeType =
    property "fill" "currentColor"
        :: vc.theme.graph.expandHandleText nodeType


nodeSeparatorToExpandHandle : Config -> NodeType -> List Style
nodeSeparatorToExpandHandle vc nodeType =
    frameStyle
        ++ vc.theme.graph.nodeSeparatorToExpandHandle nodeType


link : Config -> NodeType -> Bool -> Bool -> Maybe Color.Color -> List Style
link vc nodeType hovered selected highlight =
    [ property "stroke" "currentColor"
    , property "fill" "none"
    ]
        ++ vc.theme.graph.link nodeType hovered selected highlight


linkLabel : Config -> Bool -> Bool -> Maybe Color.Color -> List Style
linkLabel vc hovered selected color =
    (px linkLabelHeight |> fontSize)
        :: property "fill" "currentColor"
        :: vc.theme.graph.linkLabel hovered selected color


linkLabelBox : Config -> Bool -> Bool -> List Style
linkLabelBox vc hovered selected =
    vc.theme.graph.linkLabelBox hovered selected


searchTextarea : Config -> List Style
searchTextarea vc =
    vc.theme.graph.searchTextarea


toolbox : Config -> Bool -> List Style
toolbox vc visible =
    position absolute
        :: vc.theme.graph.toolbox visible


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
    vc.theme.graph.highlightColor selected


highlightTitle : Config -> List Style
highlightTitle vc =
    vc.theme.graph.highlightTitle


highlightTrash : Config -> List Style
highlightTrash vc =
    vc.theme.graph.highlightTrash
