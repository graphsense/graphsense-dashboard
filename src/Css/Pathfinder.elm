module Css.Pathfinder exposing (..)

import Config.View as View
import Css exposing (..)
import Html.Styled
import Html.Styled.Attributes as HA
import Theme.Colors as Colors
import Util.View


graphSelectionStyle : View.Config -> List Css.Style
graphSelectionStyle vc =
    [ primaryColorSelection |> Css.fill, highlightPrimaryFrostedColor vc |> border3 xsGap solid ]



-- helpers


toAttr : List Style -> Html.Styled.Attribute msg
toAttr =
    HA.css


xsGap =
    px 1


sGap =
    px 3


smGap =
    px 5


mGap =
    px 5


mlGap =
    px 10


lGap =
    px 20


all =
    pct 100


half =
    pct 50


no =
    px 0


sText =
    em 0.8


mText =
    em 1.1


lText =
    em 1.3


xlText =
    em 1.4



-- colors


type ButtonType
    = Primary
    | Secondary


primaryColorSelection : Color
primaryColorSelection =
    rgb 178 226 217


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


lightBlackColor : Color
lightBlackColor =
    rgb 33 33 33


greyColor : Color
greyColor =
    rgb 79 79 79


whiteColor : Color
whiteColor =
    rgb 255 255 255


darkBlue : Color
darkBlue =
    rgb 3 31 53


lighterDarkBlue : Color
lighterDarkBlue =
    rgb 5 50 84


greenColor : Color
greenColor =
    -- rgb 141 194 153
    hex "#369D8F"


redColor : Color
redColor =
    -- rgb 194 141 141
    hex "#E84137"



-- Styles


alertColor : View.Config -> Color
alertColor _ =
    redColor


successColor : View.Config -> Color
successColor _ =
    greenColor


highlightPrimaryColor : View.Config -> Color
highlightPrimaryColor _ =
    Colors.brandHighlight
        |> Util.View.toCssColor


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


boxStyle : View.Config -> Maybe Float -> List Style
boxStyle vc upadding =
    [ defaultBackgroundColor vc |> backgroundColor
    , boxBorderColor vc |> boxShadow5 xsGap xsGap mGap xsGap
    , px (upadding |> Maybe.withDefault 10.0) |> padding
    ]


searchInputStyle : View.Config -> String -> List Style
searchInputStyle vc _ =
    [ all |> width
    , px 20 |> height
    , display block
    , emphTextColor vc |> color
    , boxBorderColor vc |> border3 (px 1) solid
    , defaultBackgroundColor vc |> backgroundColor
    , sGap |> borderRadius
    , px 25 |> textIndent
    ]


panelHeadingStyle : View.Config -> List Style
panelHeadingStyle _ =
    [ fontWeight bold
    , xlText |> fontSize
    , mlGap |> marginBottom
    ]


panelHeadingStyle2 : View.Config -> List Style
panelHeadingStyle2 _ =
    [ fontWeight bold
    , lText |> fontSize
    , mlGap |> marginBottom
    ]


panelHeadingStyle3 : View.Config -> List Style
panelHeadingStyle3 _ =
    [ fontWeight bold
    , mText |> fontSize
    , mlGap |> marginBottom
    ]


collapsibleSectionHeadingStyle : View.Config -> List Style
collapsibleSectionHeadingStyle vc =
    [ fontWeight bold
    , mText |> fontSize
    , mlGap |> marginBottom
    , mlGap |> marginTop
    , boxBorderColor vc |> borderBottom3 (px 0.3) solid
    , px 30 |> height
    , cursor pointer
    ]


collapsibleSectionHeadingDisplaySettingsStyle : View.Config -> List Style
collapsibleSectionHeadingDisplaySettingsStyle vc =
    [ fontWeight bold
    , lText |> fontSize
    , mlGap |> marginBottom
    , mlGap |> marginTop
    , cursor pointer
    ]


toolItemSmallStyle : View.Config -> Bool -> List Style
toolItemSmallStyle vc active =
    [ px 30 |> minWidth
    , textAlign center
    , margin (px 2)
    , highlightPrimaryFrostedColor vc |> border3 xsGap solid
    , px 3 |> borderRadius
    ]
        ++ (if active then
                [ highlightPrimaryFrostedColor vc |> backgroundColor ]

            else
                []
           )


toolItemStyle : View.Config -> List Style
toolItemStyle _ =
    [ px 50 |> minWidth
    , textAlign center
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


toggleToolButtonStyle : View.Config -> Bool -> Bool -> List Style
toggleToolButtonStyle vc selected enabled =
    (textAlign center :: linkButtonStyle vc enabled)
        ++ (if selected then
                [ highlightPrimaryFrostedColor vc |> backgroundColor ]

            else
                []
           )
        ++ [ paddingTop (px 5) ]


toolButtonStyle : View.Config -> Bool -> List Style
toolButtonStyle vc enabled =
    textAlign center :: linkButtonStyle vc enabled


toolIconStyle : View.Config -> List Style
toolIconStyle _ =
    [ mText |> fontSize
    , mGap |> marginBottom
    ]


topLeftPanelStyle : View.Config -> List Style
topLeftPanelStyle _ =
    [ position absolute
    , mlGap |> left
    , mlGap |> top
    , fontSize (px 14)
    ]


graphSelectionToolsStyle : View.Config -> List Style
graphSelectionToolsStyle vc =
    [ position absolute
    , half |> left
    , px 50 |> bottom
    , displayFlex
    , transform (translate (pct -50))
    ]
        ++ boxStyle vc (Just 5)


graphActionsStyle : View.Config -> List Style
graphActionsStyle vc =
    [ displayFlex, sGap |> marginBottom ]
        ++ boxStyle vc Nothing


topRightPanelStyle : View.Config -> List Style
topRightPanelStyle _ =
    [ position absolute
    , mlGap |> right
    , mlGap |> top
    , minWidth (px 330)
    , fontSize (px 14)
    ]


searchBoxStyle : View.Config -> Maybe Float -> List Style
searchBoxStyle vc padding =
    [ px searchBoxMinWidth |> minWidth
    , sGap |> marginBottom
    ]
        ++ boxStyle vc padding


searchBoxMinWidth : Float
searchBoxMinWidth =
    150


detailsViewStyle : View.Config -> List Style
detailsViewStyle vc =
    searchBoxStyle vc (Just 0)


graphActionsViewStyle : View.Config -> List Style
graphActionsViewStyle _ =
    [ displayFlex, justifyContent flexEnd, mGap |> margin ]


graphActionButtonStyle : View.Config -> Bool -> List Style
graphActionButtonStyle vc _ =
    [ mGap |> margin
    , cursor pointer
    , padding4 sGap mlGap sGap mlGap
    , emphTextColor vc |> color
    , defaultBackgroundColor vc |> backgroundColor
    , boxBorderColor vc |> border3 xsGap solid
    , px 3 |> borderRadius
    ]


dateTimeRangeBoxStyle : View.Config -> List Style
dateTimeRangeBoxStyle vc =
    [ margin4 mGap mGap mGap mGap
    , padding mGap
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


dateTimeRangeFilterButtonStyle : View.Config -> List Style
dateTimeRangeFilterButtonStyle vc =
    [ alignItems flexEnd
    ]


detailsActionButtonStyle : View.Config -> ButtonType -> Bool -> List Style
detailsActionButtonStyle vc bt _ =
    let
        base =
            [ mGap |> margin
            , cursor pointer
            , padding4 (px 2) mlGap (px 2) mlGap
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


searchBoxContainerStyle : View.Config -> List Style
searchBoxContainerStyle _ =
    [ position relative ]


searchBoxIconStyle : View.Config -> List Style
searchBoxIconStyle _ =
    [ position absolute, px 7 |> top, px 7 |> left ]


addressDetailsViewActorImageStyle : View.Config -> List Style
addressDetailsViewActorImageStyle vc =
    [ display block
    , borderRadius (pct 50)
    , height (px 40)
    , width (px 40)
    , border3 xsGap
        solid
        (if vc.lightmode then
            blackColor

         else
            whiteColor
        )
    , mGap |> marginRight
    ]


centerContent : List Style
centerContent =
    [ displayFlex, flexDirection column, alignItems center ]


detailsViewContainerStyle : View.Config -> List Style
detailsViewContainerStyle _ =
    [ displayFlex, justifyContent left, all |> width ]


kVTableTdStyle : View.Config -> List Style
kVTableTdStyle _ =
    [ mGap |> paddingLeft ]


kVTableKeyTdStyle : View.Config -> List Style
kVTableKeyTdStyle vc =
    [ mGap |> paddingTop, mGap |> paddingBottom, emphTextColor vc |> color ] ++ kVTableTdStyle vc


kVTableValueTdStyle : View.Config -> List Style
kVTableValueTdStyle vc =
    textAlign right :: kVTableTdStyle vc ++ [ mGap |> paddingTop, mGap |> paddingBottom ]


copyableIdentifierStyle : View.Config -> List Style
copyableIdentifierStyle vc =
    [ highlightPrimaryColor vc |> color ]



-- non vc dependent styles


ruleStyle : List Style
ruleStyle =
    [ mGap |> marginBottom, mGap |> marginTop ]


inIconStyle : List Style
inIconStyle =
    [ color greenColor, ch 0.5 |> paddingRight, ch 0.2 |> paddingLeft ]


outIconStyle : List Style
outIconStyle =
    [ color redColor, ch 0.5 |> paddingRight, ch 0.2 |> paddingLeft ]


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


multiLineDatetimeDateStyle : List Style
multiLineDatetimeDateStyle =
    paddingBottom (px 4) :: dateStyle


multiLineDatetimeTimeStyle : List Style
multiLineDatetimeTimeStyle =
    [ color greyColor ]


ioOutIndicatorStyle : List Style
ioOutIndicatorStyle =
    [ ch 0.5 |> paddingLeft ]


collapsibleSectionIconStyle : List Style
collapsibleSectionIconStyle =
    [ ch 1 |> paddingRight, ch 2 |> paddingLeft ]


collapsibleSectionDisplaySettingsIconStyle : List Style
collapsibleSectionDisplaySettingsIconStyle =
    [ ch 1 |> paddingRight ]


iconWithTextStyle : List Style
iconWithTextStyle =
    [ mGap |> paddingRight ]


detailsViewCloseButtonStyle : List Style
detailsViewCloseButtonStyle =
    [ float right, margin4 mlGap mlGap no no ]


detailsContainerStyle : List Style
detailsContainerStyle =
    [ mlGap |> marginRight, mlGap |> marginLeft ]


smPaddingBottom : List Style
smPaddingBottom =
    [ paddingBottom mlGap ]


smPaddingRight : List Style
smPaddingRight =
    [ paddingRight smGap ]


fullWidth : List Style
fullWidth =
    [ all |> width ]
