module View.Pathfinder.Conversion exposing (edge, view)

import Api.Data
import Config.View as View
import Css
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Conversion exposing (Conversion)
import Msg.Pathfinder exposing (Msg)
import RecordSetter as Rs
import Svg.PathD exposing (Segment(..), pathD)
import Svg.Styled exposing (Svg, g, path)
import Svg.Styled.Attributes as Svg exposing (css)
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.GraphComponentsAggregatedTracing as Theme
import Util.TextDimensions
import View.Locale as Locale
import View.Pathfinder.Tx.Utils exposing (Pos, toPosition)


type alias Dimensions =
    { x : Float
    , y : Float
    , left : Pos
    , right : Pos
    }


calcDimensions : View.Config -> Conversion -> Address -> Address -> Dimensions
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


view : View.Config -> Conversion -> Address -> Address -> Svg Msg
view vc conversion inputAddress outputAddress =
    let
        cr =
            conversion.raw

        -- Length of horizontal extension from nodes
        labelText =
            case cr.conversionType of
                Api.Data.ExternalConversionConversionTypeDexSwap ->
                    Locale.string vc.locale "Swap:" ++ " " ++ conversion.fromAsset ++ " / " ++ conversion.toAsset

                Api.Data.ExternalConversionConversionTypeBridgeTx ->
                    Locale.string vc.locale "Bridge TX:" ++ " " ++ (cr.fromNetwork |> String.toUpper) ++ "." ++ (conversion.fromAsset |> String.toUpper) ++ " / " ++ (cr.toNetwork |> String.toUpper) ++ "." ++ (conversion.toAsset |> String.toUpper)

        horizontalExtension =
            150.0

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

        --+ curvature  -- Add curvature to bend the line
        -- Calculate position for the circular node (at the curve's peak)
        -- For cubic Bézier at t=0.5: (P0 + 3*P1 + 3*P2 + P3) / 8
        nodeX =
            (startX + 3 * control1X + 3 * control2X + endX) / 8

        nodeY =
            (startY + 3 * control1Y + 3 * control2Y + endY) / 8

        -- Node properties
        iconSize =
            GraphComponents.swapNode_details.renderedHeight

        labelOffset =
            iconSize + (Util.TextDimensions.estimateTextWidth vc.characterDimensions labelText / 2) + 2

        -- Create simple curved path using single cubic Bézier
        pat =
            pathD
                [ M ( startX, startY ) -- Start at node
                , C ( control1X, control1Y ) ( control2X, control2Y ) ( endX, endY ) -- Single curve
                ]

        hl =
            conversion.hovered

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
                        ]
                )
                {}

        -- Text label below the node
        textLabel =
            if String.isEmpty labelText then
                Svg.Styled.g [] []

            else
                Svg.Styled.text_
                    [ Svg.x (String.fromFloat (nodeX + labelOffset))
                    , Svg.y (String.fromFloat nodeY)
                    , Svg.textAnchor "middle"
                    , Svg.dominantBaseline "middle"
                    , css
                        [ Css.property "fill" Colors.black0
                        , Css.property "user-select" "none"
                        ]
                    ]
                    [ Svg.Styled.text labelText ]
    in
    g
        []
        [ -- Simple curved path
          path
            [ Svg.d pat
            , Svg.strokeDasharray "5, 5"
            , css Theme.aggregatedLinkHighlightLine_details.styles
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


edge : View.Config -> Conversion -> Address -> Address -> Svg Msg
edge vc conversion inputAddress outputAddress =
    view vc conversion inputAddress outputAddress
