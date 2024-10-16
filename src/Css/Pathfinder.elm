module Css.Pathfinder exposing
    ( annotationInputStyle
    , clusterDetailsClosedStyle
    , clusterDetailsOpenStyle
    , dateStyle
    , dateTimeRangeHighlightedDateStyle
    , datepickerButtonsStyle
    , emptyTableMsg
    , filterButtonIconStyleDateRangePicker
    , fullWidth
    , graphActionButtonStyle
    , graphActionsViewStyle
    , iconWithTextStyle
    , inIconStyle
    , inoutStyle
    , ioOutIndicatorStyle
    , kVTableKeyTdStyle
    , kVTableTdStyle
    , kVTableValueTdStyle
    , lGap
    , linkButtonStyle
    , mGap
    , mlGap
    , no
    , outIconStyle
    , sGap
    , searchBoxMinWidth
    , searchInputStyle
    , smPaddingBottom
    , smPaddingRight
    , tagConfidenceTextHighStyle
    , tagConfidenceTextLowStyle
    , tagConfidenceTextMediumStyle
    , tagLinkButtonStyle
    , tooltipMargin
    , topLeftPanelStyle
    , topPanelStyle
    , topRightPanelStyle
    )

import Config.View as View
import Css
    exposing
        ( Color
        , Pct
        , Px
        , Style
        , absolute
        , alignItems
        , backgroundColor
        , block
        , border2
        , borderRadius
        , borderWidth
        , bottom
        , calc
        , center
        , ch
        , color
        , cursor
        , display
        , displayFlex
        , ex
        , fill
        , flexEnd
        , flexGrow
        , fontSize
        , fontWeight
        , height
        , hex
        , important
        , int
        , justifyContent
        , left
        , margin
        , margin4
        , marginLeft
        , marginRight
        , minus
        , none
        , notAllowed
        , num
        , outline
        , padding
        , padding4
        , paddingBottom
        , paddingLeft
        , paddingRight
        , paddingTop
        , pct
        , pointer
        , pointerEvents
        , position
        , property
        , px
        , rgb
        , right
        , solid
        , spaceBetween
        , textAlign
        , top
        , verticalAlign
        , width
        )
import Theme.Colors as TColors


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


filterButtonIconStyleDateRangePicker : List Style
filterButtonIconStyleDateRangePicker =
    [ Css.property "fill" TColors.grey50 |> Css.important ]


clusterDetailsClosedStyle : List Style
clusterDetailsClosedStyle =
    [ Css.paddingLeft (Css.px 8)
    , Css.paddingBottom mGap
    , Css.property "color" TColors.grey50
    , Css.displayFlex
    , Css.justifyContent Css.spaceBetween
    , Css.alignItems Css.center
    , Css.cursor Css.pointer
    ]


clusterDetailsOpenStyle : List Style
clusterDetailsOpenStyle =
    [ Css.fontSize (Css.px 12)
    , Css.property "color" TColors.grey50
    , Css.marginLeft (Css.px 8)
    ]


tagConfidenceTextHighStyle : View.Config -> List Style
tagConfidenceTextHighStyle vc =
    [ color (successColor vc) ]


tagConfidenceTextMediumStyle : View.Config -> List Style
tagConfidenceTextMediumStyle vc =
    [ color (warningColor vc) ]


tagConfidenceTextLowStyle : View.Config -> List Style
tagConfidenceTextLowStyle vc =
    [ color (alertColor vc) ]


alertColor : View.Config -> Color
alertColor _ =
    redColor


successColor : View.Config -> Color
successColor _ =
    greenColor


warningColor : View.Config -> Color
warningColor _ =
    orangeColor


boxBorderColor : View.Config -> String
boxBorderColor vc =
    if vc.lightmode then
        TColors.grey50

    else
        TColors.black0


emphTextColor : View.Config -> String
emphTextColor _ =
    TColors.grey100


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
    [ width (ex 50)
    , calc (pct 100) minus (px 2) |> height
    , padding <| px 1
    , display block
    , emphTextColor vc |> property "color"
    , border2 no solid
    , outline none
    , paddingLeft (px 10)
    ]


annotationInputStyle : View.Config -> String -> List Style
annotationInputStyle vc _ =
    [ width (pct 95)
    , calc (pct 100) minus (px 2) |> height
    , padding <| px 1
    , display block
    , emphTextColor vc |> property "color"
    , border2 no solid
    , outline none
    ]


linkButtonStyle : View.Config -> Bool -> List Style
linkButtonStyle vc enabled =
    let
        clr =
            case ( vc.lightmode, enabled ) of
                ( True, True ) ->
                    TColors.black0

                ( False, True ) ->
                    TColors.white

                _ ->
                    TColors.grey50
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
    , clr |> property "color"
    ]


tagLinkButtonStyle : View.Config -> List Style
tagLinkButtonStyle _ =
    [ fontSize (px 14), no |> padding, xsGap |> paddingRight, cursor pointer, TColors.black0 |> property "color" ]


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
    , px 0 |> right
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
    , emphTextColor vc |> property "color"
    , defaultBackgroundColor vc |> backgroundColor
    , property "border" ("1px solid " ++ boxBorderColor vc)
    , px 3 |> borderRadius
    , displayFlex
    , alignItems center
    ]


datepickerButtonsStyle : View.Config -> List Style
datepickerButtonsStyle _ =
    [ Css.paddingLeft mlGap
    , Css.paddingTop mlGap, Css.displayFlex, Css.justifyContent Css.flexEnd, Css.property "gap" "5px" ]


dateTimeRangeHighlightedDateStyle : View.Config -> List Style
dateTimeRangeHighlightedDateStyle _ =
    [ TColors.greenText |> property "color"
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
