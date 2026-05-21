module View.Pathfinder.AggEdge exposing (edge, highlight, labelMetrics, view)

import Components.Tooltip as Tooltip
import Config.View as View
import Css
import Dict
import Html.Styled.Events exposing (onMouseLeave)
import Init.Pathfinder.AggEdge as AggEdge
import Maybe.Extra
import Model.Currency as Currency
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge as AggEdge exposing (AggEdge)
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter exposing (s_dividerLine, s_leftArrow, s_leftArrowGroup, s_leftValue, s_rectangleOfAggregatedLabel, s_rectangleOfHighlight, s_rightArrow, s_rightArrowGroup, s_rightEllipseOfHighlight, s_rightValue, s_root)
import RemoteData
import Svg.PathD exposing (Segment(..), pathD)
import Svg.Styled exposing (Svg, g, path)
import Svg.Styled.Attributes as Svg exposing (css, filter, transform, width)
import Svg.Styled.Events exposing (onMouseOver)
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.GraphComponentsAggregatedTracing as Theme
import Tuple exposing (mapFirst)
import Util.Graph exposing (mousedown, translate)
import Util.TextDimensions as TextDimensions
import Util.Tooltip
import Util.TooltipType as TooltipType exposing (TooltipType)
import Util.View exposing (onClickWithStop, pointer)
import View.Locale as Locale
import View.Pathfinder.Tx.Utils exposing (Pos, toPosition)


type alias Dimensions =
    { x : Float
    , y : Float
    , totalWidth : Float
    , leftLabelWidth : Float
    , rightLabelWidth : Float
    , leftLabel : String
    , rightLabel : String
    , leftVisible : Bool
    , rightVisible : Bool
    , left : Pos
    , right : Pos
    }


calcDimensions : View.Config -> { x : Float, y : Float } -> AggEdge -> Address -> Address -> Dimensions
calcDimensions vc offset ed aAddress bAddress =
    let
        padding =
            15

        -- Padding for the labels
        asset data =
            { network = data.address.currency, asset = data.address.currency }

        aPos =
            aAddress |> toPosition

        bPos =
            bAddress |> toPosition

        x =
            (aPos.x + bPos.x) / 2

        y =
            (aPos.y + bPos.y) / 2

        { leftRelation, rightRelation, left, right } =
            if aPos.x < bPos.x then
                { leftRelation = ed.b2a
                , rightRelation = ed.a2b
                , left = aPos
                , right = bPos
                }

            else
                { leftRelation = ed.a2b
                , rightRelation = ed.b2a
                , left = bPos
                , right = aPos
                }

        leftLabel =
            relationToValue leftRelation

        leftValueVisible =
            not (String.isEmpty leftLabel)

        rightValueVisible =
            not (String.isEmpty rightLabel)

        relationToValue =
            RemoteData.toMaybe
                >> Maybe.Extra.join
                >> Maybe.map
                    (\data ->
                        let
                            values =
                                ( asset data, data.value )
                                    :: (data.tokenValues
                                            |> Maybe.map Dict.toList
                                            |> Maybe.withDefault []
                                            |> List.map (mapFirst (Currency.asset data.address.currency))
                                       )
                        in
                        if data.noTxs == 0 then
                            ""

                        else
                            values
                                |> Locale.currency (View.toCurrency vc) vc.locale
                    )
                >> Maybe.withDefault ""

        rightLabel =
            relationToValue rightRelation

        leftLabelWidth =
            TextDimensions.estimateTextWidth vc.characterDimensions leftLabel
                + (if String.isEmpty leftLabel then
                    0

                   else
                    padding
                  )

        rightLabelWidth =
            TextDimensions.estimateTextWidth vc.characterDimensions rightLabel
                + (if String.isEmpty rightLabel then
                    0

                   else
                    padding
                  )

        totalWidth =
            Theme.aggregatedLabel_details.width
                - Theme.aggregatedLabelRectangleOfAggregatedLabel_details.width
                + leftLabelWidth
                + rightLabelWidth
    in
    { x = x * unit + offset.x
    , y = y * unit + offset.y
    , totalWidth = totalWidth
    , leftLabelWidth = leftLabelWidth
    , rightLabelWidth = rightLabelWidth
    , leftLabel = leftLabel
    , rightLabel = rightLabel
    , leftVisible = leftValueVisible
    , rightVisible = rightValueVisible
    , left = left
    , right = right
    }


view : View.Config -> { x : Float, y : Float } -> AggEdge -> Address -> Address -> Svg Msg
view vc offset ed aAddress bAddress =
    let
        originalWidth =
            Theme.aggregatedLabelRectangleOfAggregatedLabel_details.width

        halfOriginalWidth =
            originalWidth / 2

        { leftLabelWidth, rightLabelWidth, x, y, leftLabel, rightLabel, leftVisible, rightVisible, totalWidth } =
            calcDimensions vc offset ed aAddress bAddress

        rectangleWidth =
            leftLabelWidth + rightLabelWidth

        hidden =
            css [ Css.opacity Css.zero |> Css.important ]

        corrH =
            -2.5

        id =
            AggEdge.initId ed.a ed.b
    in
    g
        (tooltipAttributes vc id ed)
        [ Theme.aggregatedLabelWithAttributes
            (Theme.aggregatedLabelAttributes
                |> s_root
                    [ translate
                        (x - totalWidth / 2)
                        (y - (Theme.aggregatedLabel_details.height / 2))
                        |> transform
                    , id
                        |> UserMovesMouseOverAggEdge
                        |> onMouseOver
                    , mousedown (UserPushesLeftMouseButtonOnAggEdgeLabel id offset)
                    , pointer
                    ]
                |> s_rectangleOfAggregatedLabel
                    [ width <| String.fromFloat rectangleWidth
                    ]
                |> s_rectangleOfHighlight
                    [ width <| String.fromFloat rectangleWidth
                    ]
                |> s_rightArrowGroup
                    [ translate
                        (rectangleWidth - originalWidth)
                        0
                        |> transform
                    ]
                |> s_rightEllipseOfHighlight
                    [ translate
                        (rectangleWidth - originalWidth)
                        0
                        |> transform
                    ]
                |> s_leftArrow
                    (if not leftVisible then
                        [ hidden ]

                     else
                        []
                    )
                |> s_rightArrow
                    (if not rightVisible then
                        [ hidden ]

                     else
                        []
                    )
                |> s_dividerLine
                    (if leftVisible && rightVisible then
                        [ translate
                            (leftLabelWidth - halfOriginalWidth)
                            0
                            |> transform
                        ]

                     else
                        [ hidden ]
                    )
                |> s_rightValue
                    (if not leftVisible then
                        [ translate
                            -halfOriginalWidth
                            corrH
                            |> transform
                        ]

                     else
                        [ translate
                            (leftLabelWidth - halfOriginalWidth)
                            corrH
                            |> transform
                        ]
                    )
                |> s_leftValue
                    (if not rightVisible then
                        [ translate
                            (rectangleWidth - halfOriginalWidth)
                            corrH
                            |> transform
                        ]

                     else
                        [ translate
                            (leftLabelWidth - halfOriginalWidth)
                            corrH
                            |> transform
                        ]
                    )
            )
            { root =
                { leftValue = leftLabel
                , rightValue = rightLabel
                , showHighlight = False
                }
            }
        ]


highlight : View.Config -> { x : Float, y : Float } -> AggEdge -> Address -> Address -> Svg Msg
highlight vc offset ed aAddress bAddress =
    let
        originalWidth =
            Theme.aggregatedLabelRectangleOfAggregatedLabel_details.width

        { leftLabelWidth, rightLabelWidth, x, y, totalWidth } =
            calcDimensions vc offset ed aAddress bAddress

        rectangleWidth =
            leftLabelWidth + rightLabelWidth

        id =
            AggEdge.initId ed.a ed.b

        none =
            [ [ Css.display Css.none ]
                |> css
            ]
    in
    g
        [ id
            |> UserClickedAggEdge
            |> onClickWithStop
        , id
            |> UserMovesMouseOutAggEdge
            |> onMouseLeave
        , id
            |> UserMovesMouseOverAggEdge
            |> onMouseOver
        , pointer
        , filter "url(#dropShadowEdgeHighlight)"
        ]
        [ Theme.aggregatedLabelWithAttributes
            (Theme.aggregatedLabelAttributes
                |> s_root
                    [ translate
                        (x - totalWidth / 2)
                        (y - (Theme.aggregatedLabel_details.height / 2))
                        |> transform
                    ]
                |> s_rectangleOfHighlight
                    [ width <| String.fromFloat rectangleWidth
                    ]
                |> s_rightEllipseOfHighlight
                    [ translate
                        (rectangleWidth - originalWidth)
                        0
                        |> transform
                    ]
                |> s_leftArrowGroup none
                |> s_rightArrowGroup none
                |> s_dividerLine none
                |> s_rightValue none
                |> s_leftValue none
                |> s_rectangleOfAggregatedLabel none
            )
            { root =
                { leftValue = ""
                , rightValue = ""
                , showHighlight = True
                }
            }
        , edge vc offset ed aAddress bAddress True False
        , view vc offset ed aAddress bAddress
        ]


{-| The bounding box of an edge's value label, used by the placement pass to
keep labels from overlapping each other and the address nodes. Coordinates are
in pixels and `x`/`y` is the box center.
-}
labelMetrics : View.Config -> AggEdge -> Address -> Address -> { x : Float, y : Float, width : Float, height : Float }
labelMetrics vc ed aAddress bAddress =
    let
        d =
            calcDimensions vc { x = 0, y = 0 } ed aAddress bAddress
    in
    { x = d.x
    , y = d.y
    , width = d.totalWidth
    , height = Theme.aggregatedLabel_details.height
    }



{- Edge weighting disabled — it gave little visual benefit. Kept here in case
   we want to revisit it. To re-enable: uncomment this, restore the
   `import Model.Locale exposing (getFiatValue)` import, and use
   `mainStrokeWidth vc ed` for the main line's stroke-width in `edge`.

   mainStrokeWidth : View.Config -> AggEdge -> Float
   mainStrokeWidth vc ed =
       let
           fiatCode =
               String.toLower vc.preferredFiatCurrency

           fiat values =
               getFiatValue fiatCode values |> Maybe.withDefault 0

           relationFiat relation =
               relation
                   |> RemoteData.toMaybe
                   |> Maybe.Extra.join
                   |> Maybe.map
                       (\data ->
                           fiat data.value
                               + (data.tokenValues
                                   |> Maybe.map (Dict.values >> List.map fiat >> List.sum)
                                   |> Maybe.withDefault 0
                                 )
                       )
                   |> Maybe.withDefault 0

           total =
               relationFiat ed.a2b + relationFiat ed.b2a

           base =
               Theme.aggregatedLinkMainLine_details.strokeWidth
       in
       base
           + 0.65
           * logBase 10 (1 + total)
           |> clamp base 8
-}


edge : View.Config -> { x : Float, y : Float } -> AggEdge -> Address -> Address -> Bool -> Bool -> Svg Msg
edge vc offset ed aAddress bAddress hl labelGap =
    let
        { left, right, totalWidth, x, y } =
            calcDimensions vc offset ed aAddress bAddress

        fd =
            GraphComponents.addressNodeNodeFrame_details

        rad =
            fd.width / 2 + fd.strokeWidth

        ax_ =
            left.x * unit + rad

        ax =
            if ax_ > lx then
                ax_ - 2 * rad

            else
                ax_

        ay =
            left.y * unit

        bx_ =
            right.x * unit - rad

        bx =
            if bx_ < rx then
                bx_ + 2 * rad

            else
                bx_

        by =
            right.y * unit

        lx =
            x - totalWidth / 2

        rx =
            x + totalWidth / 2

        maxDiff =
            5

        diffl =
            ax
                - lx
                |> max 0

        difflCap =
            diffl
                |> min maxDiff

        diffr =
            bx
                - rx
                |> min 0

        diffrCap =
            diffr
                |> max (negate maxDiff)

        -- when the line is gapped for a label, the gap extends a little past
        -- the label box so the arrow caps don't reveal a line sliver
        -- (the label is slightly asymmetric, so left and right differ by 1px)
        gapPadL =
            if labelGap then
                4

            else
                0

        gapPadR =
            if labelGap then
                6

            else
                0

        pat =
            pathD
                [ M ( ax, ay )
                , C ( ax + (lx - ax) / 3 - diffl, y ) ( lx - difflCap - gapPadL, y ) ( lx - gapPadL, y )
                , -- a gap (move, not line) behind the label so a dimmed,
                  -- translucent label never reveals the line crossing it
                  if labelGap then
                    M ( rx + gapPadR, y )

                  else
                    L ( rx, y )
                , C ( rx - diffrCap + gapPadR, y ) ( rx + (bx - rx) / 3 * 2 - diffr, y ) ( bx, by )
                ]

        id =
            AggEdge.initId ed.a ed.b
    in
    g
        [ id
            |> UserClickedAggEdge
            |> onClickWithStop
        , id
            |> UserMovesMouseOutAggEdge
            |> onMouseLeave
        , id
            |> UserMovesMouseOverAggEdge
            |> onMouseOver
        ]
        [ path
            [ Svg.d pat
            , css Theme.aggregatedLinkHighlightLine_details.styles
            , css
                [ Css.property "stroke-width" <| String.fromFloat Theme.aggregatedLinkHighlightLine_details.strokeWidth
                , Css.property "stroke" <|
                    if hl then
                        Colors.pathAggregatedHighlight

                    else
                        "transparent"
                , Css.property "fill" "none" |> Css.important
                , Css.property "stroke-linecap" "square"
                ]
            ]
            []
        , path
            [ Svg.d pat
            , css Theme.aggregatedLinkMainLine_details.styles
            , css
                [ Css.property "stroke-width" <| String.fromFloat Theme.aggregatedLinkMainLine_details.strokeWidth
                , Css.property "stroke" Colors.pathAggregated
                , Css.property "fill" "none" |> Css.important
                , Css.property "stroke-linecap" "square"
                ]
            ]
            []
        ]
        |> List.singleton
        |> g (tooltipEventHandlers vc id ed)


tooltipAttributes : View.Config -> ( Id, Id ) -> AggEdge -> List (Svg.Styled.Attribute Msg)
tooltipAttributes vc id ed =
    aggEdgeToTooltipType ed
        |> Maybe.map
            (Tooltip.attributes (AggEdge.idToString id)
                (Util.Tooltip.tooltipConfig vc TooltipMsg
                    |> Tooltip.withOpenDelay 100
                )
            )
        |> Maybe.withDefault []


tooltipEventHandlers : View.Config -> ( Id, Id ) -> AggEdge -> List (Svg.Styled.Attribute Msg)
tooltipEventHandlers vc id ed =
    aggEdgeToTooltipType ed
        |> Maybe.map
            (Tooltip.eventHandlers (AggEdge.idToString id)
                (Util.Tooltip.tooltipConfig vc TooltipMsg
                    |> Tooltip.withOpenDelay 100
                )
            )
        |> Maybe.withDefault []


aggEdgeToTooltipType : AggEdge -> Maybe TooltipType
aggEdgeToTooltipType ed =
    Maybe.map4
        (\a b a2b b2a ->
            if a.x < b.x then
                { leftAddress = a.id
                , left = a2b
                , rightAddress = b.id
                , right = b2a
                }

            else
                { leftAddress = b.id
                , left = b2a
                , rightAddress = a.id
                , right = a2b
                }
        )
        ed.aAddress
        ed.bAddress
        (RemoteData.toMaybe ed.a2b)
        (RemoteData.toMaybe ed.b2a)
        |> Maybe.map TooltipType.AggEdge
