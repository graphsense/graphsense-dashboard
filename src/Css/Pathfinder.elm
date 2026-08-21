module Css.Pathfinder exposing
    ( bottomCenterPanelStyle
    , emptyTableMsg
    , fullWidth
    , graphActionsViewStyle
    , inoutStyle
    , mGap
    , plainLinkStyle
    , searchBoxMinWidth
    , sidePanelCss
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
        , bottom
        , center
        , color
        , displayFlex
        , flexEnd
        , important
        , justifyContent
        , margin
        , marginLeft
        , marginRight
        , none
        , paddingRight
        , pct
        , pointerEvents
        , position
        , property
        , px
        , right
        , spaceBetween
        , textAlign
        , top
        , width
        )
import Theme.Colors as TColors
import Util.View


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



-- colors
-- rgb 141 194 153
-- hex "#369D8F"


redColor : Color
redColor =
    -- rgb 194 141 141
    -- hex "#E84137"
    TColors.pathIn_color |> Util.View.toCssColor



-- Styles


plainLinkStyle : View.Config -> List Style
plainLinkStyle _ =
    [ TColors.black0 |> property "color" ]


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
    , Css.flexWrap Css.wrap
    , Css.property "row-gap" "10px"
    , Css.property "column-gap" "10px"
    ]


bottomCenterPanelStyle : List Style
bottomCenterPanelStyle =
    [ position absolute
    , Css.px 50 |> bottom
    , displayFlex
    , justifyContent center
    , width all
    , pointerEvents none
    ]


topRightPanelStyle : View.Config -> List Style
topRightPanelStyle _ =
    [ position absolute
    , px 0 |> right
    , top (px topRightPanelY)
    ]


topRightPanelY : Float
topRightPanelY =
    70


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
    [ all |> width |> important ]


sidePanelCss : List Css.Style
sidePanelCss =
    [ Css.calc (Css.vh 100) Css.minus (Css.px <| topRightPanelY) |> Css.maxHeight
    , Css.overflowY Css.auto
    , Css.overflowX Css.hidden
    ]
