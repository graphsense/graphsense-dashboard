module Css.Pathfinder exposing (..)

import Config.View as View
import Css exposing (..)
import Html.Styled
import Html.Styled.Attributes as HA exposing (..)


addressRoot : View.Config -> List Style
addressRoot vc =
    vc.theme.pathfinder.addressRoot



-- helpers


toAttr : List Style -> Html.Styled.Attribute msg
toAttr =
    HA.css



-- colors


type ButtonType
    = Primary
    | Secondary


primaryColor : Css.Color
primaryColor =
    Css.rgb 26 197 176


primaryFrostedColor : Css.Color
primaryFrostedColor =
    Css.rgb 26 197 176


lighterGreyColor : Css.Color
lighterGreyColor =
    Css.rgb 208 216 220


lightGreyColor : Css.Color
lightGreyColor =
    Css.rgb 120 144 156


blackColor : Css.Color
blackColor =
    Css.rgb 0 0 0


whiteColor : Css.Color
whiteColor =
    Css.rgb 255 255 255


darkBlue : Css.Color
darkBlue =
    Css.rgb 3 31 53


lighterDarkBlue : Css.Color
lighterDarkBlue =
    Css.rgb 5 50 84


greenColor : Css.Color
greenColor =
    Css.rgb 141 194 153


redColor : Css.Color
redColor =
    Css.rgb 194 141 141



-- Styles
-- http://probablyprogramming.com/2009/03/15/the-tiniest-gif-ever


alertColor : View.Config -> Css.Color
alertColor _ =
    redColor


successColor : View.Config -> Css.Color
successColor _ =
    greenColor


highlightPrimaryColor : View.Config -> Css.Color
highlightPrimaryColor _ =
    primaryColor


highlightPrimaryFrostedColor : View.Config -> Css.Color
highlightPrimaryFrostedColor _ =
    primaryFrostedColor


boxBorderColor : View.Config -> Css.Color
boxBorderColor vc =
    if vc.lightmode then
        lighterGreyColor

    else
        blackColor


emphTextColor : View.Config -> Css.Color
emphTextColor _ =
    lightGreyColor


defaultBackgroundColor : View.Config -> Css.Color
defaultBackgroundColor vc =
    if vc.lightmode then
        whiteColor

    else
        darkBlue


boxStyle : View.Config -> Maybe Float -> List Css.Style
boxStyle vc padding =
    [ defaultBackgroundColor vc |> Css.backgroundColor
    , boxBorderColor vc |> Css.boxShadow5 (Css.px 1) (Css.px 1) (Css.px 5) (Css.px 1)
    , Css.px (padding |> Maybe.withDefault 10) |> Css.padding
    ]


searchInputStyle : View.Config -> String -> List Css.Style
searchInputStyle vc _ =
    [ Css.pct 100 |> Css.width
    , Css.px 20 |> Css.height
    , Css.display Css.block
    , emphTextColor vc |> Css.color
    , boxBorderColor vc |> Css.border3 (Css.px 1) Css.solid
    , defaultBackgroundColor vc |> Css.backgroundColor
    , Css.px 3 |> Css.borderRadius
    , Css.px 25 |> Css.textIndent
    ]


panelHeadingStyle : View.Config -> List Css.Style
panelHeadingStyle _ =
    [ Css.fontWeight Css.bold
    , Css.em 1.4 |> Css.fontSize
    , Css.px 10 |> Css.marginBottom
    ]


panelHeadingStyle2 : View.Config -> List Css.Style
panelHeadingStyle2 _ =
    [ Css.fontWeight Css.bold
    , Css.em 1.3 |> Css.fontSize
    , Css.px 10 |> Css.marginBottom
    ]


collapsibleSectionHeadingStyle : View.Config -> List Css.Style
collapsibleSectionHeadingStyle vc =
    [ Css.fontWeight Css.bold
    , Css.em 1.1 |> Css.fontSize
    , Css.px 10 |> Css.marginBottom
    , Css.px 10 |> Css.marginTop
    , boxBorderColor vc |> Css.borderBottom3 (Css.px 0.3) Css.solid
    , Css.px 30 |> Css.height
    , Css.cursor Css.pointer
    ]


toolItemStyle : View.Config -> List Css.Style
toolItemStyle _ =
    [ Css.px 55 |> Css.minWidth
    , Css.textAlign Css.center
    ]


linkButtonStyle : View.Config -> Bool -> List Css.Style
linkButtonStyle vc enabled =
    [ defaultBackgroundColor vc |> Css.backgroundColor
    , Css.px 0 |> Css.borderWidth
    , Css.cursor Css.pointer
    , Css.px 0 |> Css.padding
    , Css.px 5 |> Css.paddingLeft
    , Css.color
        (if vc.lightmode then
            blackColor

         else
            whiteColor
        )
    ]


toolButtonStyle : View.Config -> Bool -> List Css.Style
toolButtonStyle vc enabled =
    Css.textAlign Css.center :: linkButtonStyle vc enabled


toolIconStyle : View.Config -> List Css.Style
toolIconStyle _ =
    [ Css.em 1.3 |> Css.fontSize
    , Css.px 5 |> Css.marginBottom
    ]


topLeftPanelStyle : View.Config -> List Css.Style
topLeftPanelStyle _ =
    [ Css.position Css.absolute
    , Css.px 10 |> Css.left
    , Css.px 10 |> Css.top
    ]


graphToolsStyle : View.Config -> List Css.Style
graphToolsStyle vc =
    [ Css.position Css.absolute
    , Css.pct 50 |> Css.left
    , Css.px 50 |> Css.bottom
    , Css.displayFlex
    , Css.transform (Css.translate (Css.pct -50))
    ]
        ++ boxStyle vc Nothing


topRightPanelStyle : View.Config -> List Css.Style
topRightPanelStyle _ =
    [ Css.position Css.absolute
    , Css.px 10 |> Css.right
    , Css.px 10 |> Css.top
    ]


searchBoxStyle : View.Config -> Maybe Float -> List Css.Style
searchBoxStyle vc padding =
    [ Css.px 300 |> Css.minWidth
    , Css.px 10 |> Css.marginBottom
    ]
        ++ boxStyle vc padding


detailsViewStyle : View.Config -> List Css.Style
detailsViewStyle vc =
    searchBoxStyle vc (Just 0)


graphActionsViewStyle : View.Config -> List Css.Style
graphActionsViewStyle _ =
    [ Css.displayFlex, Css.justifyContent Css.flexEnd, Css.px 5 |> Css.margin ]


graphActionButtonStyle : View.Config -> Bool -> List Css.Style
graphActionButtonStyle vc _ =
    [ Css.px 5 |> Css.margin
    , Css.cursor Css.pointer
    , Css.padding4 (Css.px 3) (Css.px 10) (Css.px 3) (Css.px 10)
    , emphTextColor vc |> Css.color
    , defaultBackgroundColor vc |> Css.backgroundColor
    , boxBorderColor vc |> Css.border3 (Css.px 1) Css.solid
    , Css.px 3 |> Css.borderRadius
    ]


detailsActionButtonStyle : View.Config -> ButtonType -> Bool -> List Css.Style
detailsActionButtonStyle vc bt _ =
    case bt of
        Primary ->
            [ Css.px 5 |> Css.margin
            , Css.cursor Css.pointer
            , Css.padding4 (Css.px 4) (Css.px 10) (Css.px 4) (Css.px 10)
            , Css.color whiteColor
            , Css.fontWeight Css.bold
            , highlightPrimaryFrostedColor vc |> Css.backgroundColor
            , highlightPrimaryFrostedColor vc |> Css.border3 (Css.px 1) Css.solid
            , Css.px 3 |> Css.borderRadius
            ]

        Secondary ->
            [ Css.px 5 |> Css.margin
            , Css.cursor Css.pointer
            , Css.padding4 (Css.px 4) (Css.px 10) (Css.px 4) (Css.px 10)
            , highlightPrimaryFrostedColor vc |> Css.color
            , Css.fontWeight Css.bold
            , Css.backgroundColor whiteColor
            , highlightPrimaryFrostedColor vc |> Css.border3 (Css.px 1) Css.solid
            , Css.px 3 |> Css.borderRadius
            ]


searchViewStyle : View.Config -> List Css.Style
searchViewStyle vc =
    boxStyle vc Nothing ++ [ Css.displayFlex, Css.justifyContent Css.flexEnd ]


searchBoxContainerStyle : View.Config -> List Css.Style
searchBoxContainerStyle _ =
    [ Css.position Css.relative ]


searchBoxIconStyle : View.Config -> List Css.Style
searchBoxIconStyle _ =
    [ Css.position Css.absolute, Css.px 7 |> Css.top, Css.px 7 |> Css.left ]


addressDetailsViewActorImageStyle : View.Config -> List Css.Style
addressDetailsViewActorImageStyle vc =
    [ Css.display Css.block
    , Css.borderRadius (Css.pct 50)
    , Css.height (Css.px 40)
    , Css.width (Css.px 40)
    , Css.border3 (Css.px 1)
        Css.solid
        (if vc.lightmode then
            blackColor

         else
            whiteColor
        )
    , Css.px 5 |> Css.marginRight
    ]


detailsViewContainerStyle : View.Config -> List Css.Style
detailsViewContainerStyle _ =
    [ Css.displayFlex, Css.justifyContent Css.left, Css.pct 100 |> Css.width ]


kVTableTdStyle : View.Config -> List Css.Style
kVTableTdStyle _ =
    [ Css.px 5 |> Css.paddingLeft ]


kVTableKeyTdStyle : View.Config -> List Css.Style
kVTableKeyTdStyle vc =
    [ Css.px 5 |> Css.paddingTop, Css.px 5 |> Css.paddingBottom, emphTextColor vc |> Css.color ] ++ kVTableTdStyle vc


kVTableValueTdStyle : View.Config -> List Css.Style
kVTableValueTdStyle vc =
    Css.textAlign Css.right :: kVTableTdStyle vc


copyableIdentifierStyle : View.Config -> List Css.Style
copyableIdentifierStyle vc =
    [ highlightPrimaryColor vc |> Css.color ]



-- non vc dependent styles


ruleStyle : List Css.Style
ruleStyle =
    [ Css.px 5 |> Css.marginBottom, Css.px 5 |> Css.marginTop ]


inIconStyle : List Css.Style
inIconStyle =
    [ Css.color greenColor, Css.ch 0.5 |> Css.paddingRight, Css.ch 0.2 |> Css.paddingLeft ]


outIconStyle : List Css.Style
outIconStyle =
    [ Css.color redColor, Css.ch 0.5 |> Css.paddingRight, Css.ch 0.2 |> Css.paddingLeft ]


ioOutIndicatorStyle : List Css.Style
ioOutIndicatorStyle =
    [ Css.ch 0.5 |> Css.paddingLeft ]


collapsibleSectionIconStyle : List Css.Style
collapsibleSectionIconStyle =
    [ Css.ch 1 |> Css.paddingRight, Css.ch 2 |> Css.paddingLeft ]


iconWithTextStyle : List Css.Style
iconWithTextStyle =
    [ Css.px 5 |> Css.paddingRight ]


detailsViewCloseButtonStyle : List Css.Style
detailsViewCloseButtonStyle =
    [ Css.float Css.right, Css.margin4 (Css.px 10) (Css.px 10) (Css.px 0) (Css.px 0) ]


addressDetailsContainerStyle : List Css.Style
addressDetailsContainerStyle =
    [ Css.px 10 |> Css.marginRight, Css.px 10 |> Css.marginLeft ]


smPaddingBottom : List Css.Style
smPaddingBottom =
    [ Css.paddingBottom (Css.px 10) ]


fullWidth : List Css.Style
fullWidth =
    [ Css.pct 100 |> Css.width ]
