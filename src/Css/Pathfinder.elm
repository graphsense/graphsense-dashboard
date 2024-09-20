module Css.Pathfinder exposing (..)

import Config.View as View
import Css exposing (..)
import Html.Styled
import Html.Styled.Attributes as HA


toAttr : List Style -> Html.Styled.Attribute msg
toAttr =
    HA.css


xsGap : Px
xsGap =
    px 1


sGap : Px
sGap =
    px 3


smGap : Px
smGap =
    px 5


mGap : Px
mGap =
    px 5


mlGap : Px
mlGap =
    px 10


lGap : Px
lGap =
    px 20


all : Pct
all =
    pct 100


no : Px
no =
    px 0



-- colors


type ButtonType
    = Primary
    | Secondary


primaryFrostedColor : Color
primaryFrostedColor =
    rgb 107 203 186


lighterGreyColor : Color
lighterGreyColor =
    rgb 208 216 220


lightGreyColor : Color
lightGreyColor =
    rgb 120 144 156


blackColor : Color
blackColor =
    rgb 33 33 33


whiteColor : Color
whiteColor =
    rgb 255 255 255


darkBlue : Color
darkBlue =
    rgb 3 31 53


greenColor : Color
greenColor =
    -- rgb 141 194 153
    hex "#369D8F"


redColor : Color
redColor =
    -- rgb 194 141 141
    hex "#E84137"


orangeColor : Color
orangeColor =
    -- rgb 194 141 141
    hex "#FF9800"



-- Styles


alertColor : View.Config -> Color
alertColor _ =
    redColor


successColor : View.Config -> Color
successColor _ =
    greenColor


warningColor : View.Config -> Color
warningColor _ =
    orangeColor


highlightPrimaryFrostedColor : View.Config -> Color
highlightPrimaryFrostedColor _ =
    primaryFrostedColor


boxBorderColor : View.Config -> Color
boxBorderColor vc =
    if vc.lightmode then
        lighterGreyColor

    else
        blackColor


emphTextColor : View.Config -> Color
emphTextColor _ =
    lightGreyColor


defaultBackgroundColor : View.Config -> Color
defaultBackgroundColor vc =
    if vc.lightmode then
        whiteColor

    else
        darkBlue


tooltipMargin : List Style
tooltipMargin =
    [ margin4 sGap mGap sGap mGap ]


searchInputStyle : View.Config -> String -> List Style
searchInputStyle vc _ =
    [ width (ex 40)
    , calc (pct 100) minus (px 2) |> height
    , padding <| px 1
    , display block
    , emphTextColor vc |> color
    , boxBorderColor vc |> border3 no solid
    , outline none
    ]


panelHeadingStyle3 : View.Config -> List Style
panelHeadingStyle3 _ =
    [ fontWeight bold
    , Css.marginTop (Css.px 15)
    , mlGap |> marginBottom
    ]


linkButtonStyle : View.Config -> Bool -> List Style
linkButtonStyle vc enabled =
    let
        clr =
            case ( vc.lightmode, enabled ) of
                ( True, True ) ->
                    blackColor

                ( False, True ) ->
                    whiteColor

                _ ->
                    lighterGreyColor
    in
    [ defaultBackgroundColor vc |> backgroundColor
    , no |> borderWidth
    , cursor
        (if enabled then
            pointer

         else
            notAllowed
        )
    , no |> padding
    , mGap |> paddingLeft
    , mGap |> paddingRight
    , clr |> color
    ]


tagLinkButtonStyle : View.Config -> List Style
tagLinkButtonStyle vc =
    [ fontSize (px 14), no |> padding, xsGap |> paddingRight, cursor pointer, blackColor |> color ]


topLeftPanelStyle : View.Config -> List Style
topLeftPanelStyle _ =
    [ position absolute
    , mlGap |> left
    , mlGap |> top
    , fontSize (px 14)
    ]


topPanelStyle : List Style
topPanelStyle =
    [ position absolute
    , marginLeft mlGap
    , marginRight mlGap
    , mlGap |> top
    , displayFlex
    , alignItems center
    , width all
    , pointerEvents none
    , justifyContent spaceBetween
    ]


topRightPanelStyle : View.Config -> List Style
topRightPanelStyle _ =
    [ position absolute
    , mlGap |> right
    , top (px 70)
    ]


searchBoxMinWidth : Float
searchBoxMinWidth =
    150


graphActionsViewStyle : View.Config -> List Style
graphActionsViewStyle _ =
    [ displayFlex, justifyContent flexEnd, paddingRight lGap ]


graphActionButtonStyle : View.Config -> Bool -> List Style
graphActionButtonStyle vc _ =
    [ cursor pointer
    , padding4 xsGap mlGap xsGap mlGap
    , emphTextColor vc |> color
    , defaultBackgroundColor vc |> backgroundColor
    , boxBorderColor vc |> border3 xsGap solid
    , px 3 |> borderRadius
    , displayFlex
    , alignItems center
    ]


dateTimeRangeBoxStyle : View.Config -> List Style
dateTimeRangeBoxStyle vc =
    [ padding4 xsGap mGap xsGap mGap
    , marginRight sGap
    , marginLeft sGap
    , emphTextColor vc |> color
    , defaultBackgroundColor vc |> backgroundColor
    , boxBorderColor vc |> border3 xsGap solid
    , px 3 |> borderRadius
    , displayFlex
    , alignItems center
    , justifyContent spaceBetween
    , flexGrow <| num 1
    ]


dateTimeRangeHighlightedDateStyle : View.Config -> List Style
dateTimeRangeHighlightedDateStyle vc =
    -- [ padding4 zero mGap  zero mGap
    [ primaryFrostedColor |> color
    ]


detailsActionButtonStyle : View.Config -> ButtonType -> Bool -> List Style
detailsActionButtonStyle vc bt _ =
    let
        base =
            [ mGap |> marginRight
            , cursor pointer
            , fontWeight bold
            , px 3 |> borderRadius
            ]
    in
    case bt of
        Primary ->
            base
                ++ [ color whiteColor
                   , fontWeight bold
                   , highlightPrimaryFrostedColor vc |> backgroundColor
                   , highlightPrimaryFrostedColor vc |> border3 xsGap solid
                   ]

        Secondary ->
            base
                ++ [ highlightPrimaryFrostedColor vc |> color
                   , backgroundColor whiteColor
                   , highlightPrimaryFrostedColor vc |> border3 xsGap solid
                   ]


emptyTableMsg : List Style
emptyTableMsg =
    [ margin (px 20), textAlign center ]


kVTableTdStyle : View.Config -> List Style
kVTableTdStyle _ =
    [ mGap |> paddingLeft ]


kVTableKeyTdStyle : View.Config -> List Style
kVTableKeyTdStyle vc =
    [ mGap |> paddingTop, mGap |> paddingBottom ] ++ kVTableTdStyle vc


kVTableValueTdStyle : View.Config -> List Style
kVTableValueTdStyle vc =
    textAlign right :: kVTableTdStyle vc ++ [ mGap |> paddingTop, mGap |> paddingBottom ]



-- non vc dependent styles


inIconStyle : List Style
inIconStyle =
    [ fill greenColor, verticalAlign bottom ] |> List.map important


outIconStyle : List Style
outIconStyle =
    [ fill redColor, verticalAlign bottom ] |> List.map important


inoutStyle : Bool -> List Style
inoutStyle out =
    [ if out then
        color redColor

      else
        color greenColor
    ]


dateStyle : List Style
dateStyle =
    [ fontWeight (int 600) ]


ioOutIndicatorStyle : List Style
ioOutIndicatorStyle =
    [ ch 0.5 |> paddingLeft ]


iconWithTextStyle : List Style
iconWithTextStyle =
    [ mGap |> paddingRight ]


smPaddingBottom : List Style
smPaddingBottom =
    [ paddingBottom mlGap ]


smPaddingRight : List Style
smPaddingRight =
    [ paddingRight smGap ]


fullWidth : List Style
fullWidth =
    [ all |> width ]
