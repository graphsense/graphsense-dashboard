module Css.Pathfinder exposing
    ( annotationInputStyle
    , emptyTableMsg
    , fullWidth
    , graphActionsViewStyle
    , inoutStyle
    , lGap
    , linkButtonStyle
    , mGap
    , mlGap
    , no
    , plainLinkStyle
    , sGap
    , searchBoxMinWidth
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
        , block
        , border2
        , borderWidth
        , calc
        , center
        , color
        , cursor
        , display
        , displayFlex
        , flexEnd
        , fontSize
        , height
        , hex
        , justifyContent
        , left
        , margin
        , margin4
        , marginLeft
        , marginRight
        , minus
        , none
        , notAllowed
        , outline
        , padding
        , paddingLeft
        , paddingRight
        , pct
        , pointer
        , pointerEvents
        , position
        , property
        , px
        , right
        , solid
        , spaceBetween
        , textAlign
        , top
        , width
        )
import Theme.Colors as TColors
import Util.View


xsGap : Px
xsGap =
    px 1


sGap : Px
sGap =
    px 3


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


greenColor : Color
greenColor =
    TColors.pathOut_color |> Util.View.toCssColor



-- rgb 141 194 153
-- hex "#369D8F"


redColor : Color
redColor =
    -- rgb 194 141 141
    -- hex "#E84137"
    TColors.pathIn_color |> Util.View.toCssColor


orangeColor : Color
orangeColor =
    -- rgb 194 141 141
    hex "#FF9800"



-- Styles


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


emphTextColor : View.Config -> String
emphTextColor _ =
    TColors.grey100


tooltipMargin : List Style
tooltipMargin =
    [ margin4 sGap mGap sGap mGap ]


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


plainLinkStyle : View.Config -> List Style
plainLinkStyle _ =
    [ TColors.black0 |> property "color" ]


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
    [ no |> borderWidth
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


emptyTableMsg : List Style
emptyTableMsg =
    [ margin (px 20), textAlign center ]



-- non vc dependent styles


inoutStyle : Bool -> List Style
inoutStyle out =
    if out then
        [ color redColor
        ]

    else
        []



--   else
-- color greenColor


fullWidth : List Style
fullWidth =
    [ all |> width ]
