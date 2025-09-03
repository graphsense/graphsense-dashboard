module View.Pathfinder.ConversionEdge exposing (edge, view)

import Api.Data
import Config.View as View
import Css
import Html.Styled.Events exposing (onMouseLeave)
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter as Rs
import Svg.PathD exposing (Segment(..), pathD)
import Svg.Styled exposing (Svg, g, path)
import Svg.Styled.Attributes as Svg exposing (css, filter)
import Svg.Styled.Events exposing (onMouseOver)
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.GraphComponentsAggregatedTracing as Theme
import Util.TextDimensions
import Util.View exposing (onClickWithStop, pointer)
import View.Locale as Locale
import View.Pathfinder.Tx.Utils exposing (Pos, toPosition)


type alias Dimensions =
    { x : Float
    , y : Float
    , left : Pos
    , right : Pos
    }


calcDimensions : View.Config -> ConversionEdge -> Address -> Address -> Dimensions
calcDimensions _ _ aAddress bAddress =
    let
        -- Padding for the labels
        aPos =
            aAddress |> toPosition

        bPos =
            bAddress |> toPosition

        x =
            (aPos.x + bPos.x) / 2

        y =
            (aPos.y + bPos.y) / 2

        { left, right } =
            if aPos.x < bPos.x then
                { left = aPos
                , right = bPos
                }

            else
                { left = bPos
                , right = aPos
                }
    in
    { x = x * unit
    , y = y * unit
    , left = left
    , right = right
    }


view : View.Config -> ConversionEdge -> Int -> Address -> Address -> Svg Msg
view vc conversion displacementIndex inputAddress outputAddress =
    let
        cr =
            conversion.raw

        id =
            conversion.id

        -- Length of horizontal extension from nodes
        labelTextLine1 =
            case cr.conversionType of
                Api.Data.ExternalConversionConversionTypeDexSwap ->
                    Locale.string vc.locale "Swap"

                Api.Data.ExternalConversionConversionTypeBridgeTx ->
                    Locale.string vc.locale "Bridge TX"

        labelTextLine2 =
            case cr.conversionType of
                Api.Data.ExternalConversionConversionTypeDexSwap ->
                    conversion.fromAsset ++ " / " ++ conversion.toAsset

                Api.Data.ExternalConversionConversionTypeBridgeTx ->
                    (cr.fromNetwork |> String.toUpper) ++ "-" ++ (conversion.fromAsset |> String.toUpper) ++ " / " ++ (cr.toNetwork |> String.toUpper) ++ "-" ++ (conversion.toAsset |> String.toUpper)

        horizontalExtension =
            150.0 + (30 * toFloat displacementIndex)

        -- Teardrop loop parameters
        loopXDisplacement =
            80.0

        -- Horizontal extension for teardrop
        loopYDisplacement =
            40.0 + (30 * toFloat displacementIndex)

        -- Vertical displacement based on displacement index
        { left, right } =
            calcDimensions vc conversion inputAddress outputAddress

        fd =
            GraphComponents.addressNodeNodeFrame_details

        rad =
            fd.width / 2 + fd.strokeWidth

        -- Always attach to the right side of both nodes
        startX =
            left.x * unit + rad

        startY =
            left.y * unit

        endX =
            right.x * unit + rad

        endY =
            right.y * unit

        -- Check if start and end points are the same
        isSamePoint =
            abs (startX - endX) < 1.0 && abs (startY - endY) < 1.0

        -- Create path - either loop or curve
        pat =
            if isSamePoint then
                -- Create a teardrop-shaped loop with round head
                let
                    -- Teardrop tip position
                    tipX =
                        startX + (loopXDisplacement * 1.2)

                    tipY =
                        startY - loopYDisplacement

                    -- Control points for smooth teardrop shape
                    control1X =
                        startX + (loopXDisplacement * 0.7)

                    -- Gentle outward curve
                    control1Y =
                        startY - (loopYDisplacement * 0.2)

                    -- Slight upward
                    control2X =
                        startX + (loopXDisplacement * 1.1)

                    -- Near the tip
                    control2Y =
                        startY - (loopYDisplacement * 0.8)

                    -- Close to tip height
                    -- Return curve control points
                    control3X =
                        startX + (loopXDisplacement * 1.1)

                    -- Mirror of control2X
                    control3Y =
                        startY - (loopYDisplacement * 1.2)

                    -- Above the tip for round shape
                    control4X =
                        startX + (loopXDisplacement * 0.3)

                    -- Gentle return
                    control4Y =
                        startY - (loopYDisplacement * 0.4)

                    -- Smooth back to start
                in
                pathD
                    [ M ( startX, startY ) -- Start at the node
                    , C ( control1X, control1Y ) ( control2X, control2Y ) ( tipX, tipY ) -- First curve to tip
                    , C ( control3X, control3Y ) ( control4X, control4Y ) ( startX, startY ) -- Return curve to start
                    ]

            else
                -- Original curve path (unchanged)
                let
                    -- Calculate control points for cubic Bézier curve
                    -- First control point - extend horizontally to the right from start
                    control1X =
                        startX + horizontalExtension

                    control1Y =
                        startY

                    -- Second control point - extend horizontally to the right from end, with curvature offset
                    control2X =
                        endX + horizontalExtension

                    control2Y =
                        endY
                in
                pathD
                    [ M ( startX, startY ) -- Start at node
                    , C ( control1X, control1Y ) ( control2X, control2Y ) ( endX, endY ) -- Single curve
                    ]

        -- Calculate node position
        ( nodeX, nodeY ) =
            if isSamePoint then
                -- Position node at the tip of the teardrop
                ( startX + (loopXDisplacement * 1.2), startY - loopYDisplacement )

            else
                -- Original calculation for curve (unchanged)
                let
                    control1X =
                        startX + horizontalExtension

                    control1Y =
                        startY

                    control2X =
                        endX + horizontalExtension

                    control2Y =
                        endY
                in
                ( (startX + 3 * control1X + 3 * control2X + endX) / 8
                , (startY + 3 * control1Y + 3 * control2Y + endY) / 8
                )

        -- Node properties
        iconSize =
            GraphComponents.swapNode_details.renderedHeight

        hl =
            conversion.hovered || conversion.selected

        swapNode =
            GraphComponents.swapNodeWithAttributes
                (GraphComponents.swapNodeAttributes
                    |> Rs.s_root
                        [ Svg.transform
                            ("translate("
                                ++ String.fromFloat (nodeX - iconSize / 2)
                                ++ ","
                                ++ String.fromFloat (nodeY - iconSize / 2)
                                ++ ")"
                            )
                        , UserMovesMouseOutConversionEdge id conversion
                            |> onMouseLeave
                        , UserMovesMouseOverConversionEdge id conversion
                            |> onMouseOver
                        , pointer
                        , [ Css.property "stroke" <|
                                if hl then
                                    Colors.pathAggregatedHighlight

                                else
                                    "transparent"
                          ]
                            |> css
                        , filter "url(#dropShadowAggEdgeHighlight)"
                        ]
                )
                {}

        -- Text label below the node
        labelOffsetLine1 =
            iconSize + (Util.TextDimensions.estimateTextWidth vc.characterDimensions labelTextLine1 / 2) + 2

        labelOffsetLine2 =
            iconSize + (Util.TextDimensions.estimateTextWidth vc.characterDimensions labelTextLine2 / 2) + 2

        lableOffset =
            max labelOffsetLine1 labelOffsetLine2

        textLabel =
            if String.isEmpty labelTextLine1 then
                Svg.Styled.g [] []

            else
                Svg.Styled.g
                    [ css
                        [ Css.property "fill" Colors.black0
                        , Css.property "user-select" "none"
                        , Css.fontSize (Css.px 12)
                        ]
                    ]
                    [ Svg.Styled.text_
                        [ Svg.x (String.fromFloat (nodeX + lableOffset))
                        , Svg.y (String.fromFloat (nodeY - 7))
                        , Svg.textAnchor "middle"
                        , Svg.dominantBaseline "middle"
                        , css
                            [ Css.fontWeight (Css.int 600)
                            ]
                        ]
                        [ Svg.Styled.text labelTextLine1 ]
                    , Svg.Styled.text_
                        [ Svg.x (String.fromFloat (nodeX + lableOffset))
                        , Svg.y (String.fromFloat (nodeY + 7))
                        , Svg.textAnchor "middle"
                        , Svg.dominantBaseline "middle"
                        , css
                            []
                        ]
                        [ Svg.Styled.text labelTextLine2 ]
                    ]
    in
    g
        [ UserClickedConversionEdge id conversion
            |> onClickWithStop
        , UserMovesMouseOutConversionEdge id conversion
            |> onMouseLeave
        , UserMovesMouseOverConversionEdge id conversion
            |> onMouseOver
        ]
        [ -- Simple curved path or loop
          path
            [ Svg.d pat

            -- , Svg.strokeDasharray "5, 5"
            , css Theme.aggregatedLinkHighlightLine_details.styles
            , pointer
            , css
                [ Css.property "stroke-width" <| String.fromFloat Theme.aggregatedLinkHighlightLine_details.strokeWidth
                , Css.property "stroke" <|
                    if hl then
                        Colors.pathAggregatedHighlight

                    else
                        "transparent"
                , Css.property "fill" "none" |> Css.important
                , Css.property "stroke-linecap" "round"
                ]
            ]
            []
        , path
            [ Svg.d pat
            , Svg.strokeDasharray "5, 5"
            , css Theme.aggregatedLinkMainLine_details.styles
            , pointer
            , css
                [ Css.property "stroke-width" <| String.fromFloat Theme.aggregatedLinkMainLine_details.strokeWidth
                , Css.property "stroke" Colors.pathMiddle
                , Css.property "fill" "none" |> Css.important
                , Css.property "stroke-linecap" "round"
                ]
            ]
            []

        -- Circular node
        , swapNode

        -- Text label
        , textLabel
        ]



-- Keep the original edge function for backward compatibility with default curvature


edge : View.Config -> ConversionEdge -> Int -> Address -> Address -> Svg Msg
edge vc conversionEdge displacementIndex inputAddress outputAddress =
    view vc conversionEdge displacementIndex inputAddress outputAddress
