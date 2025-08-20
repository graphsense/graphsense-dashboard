module View.Pathfinder.Conversion exposing (edge, view)

import Api.Data
import Config.View as View
import Css
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Conversion exposing (Conversion)
import Msg.Pathfinder exposing (Msg)
import Svg.PathD exposing (Segment(..), pathD)
import Svg.Styled exposing (Svg, g, path)
import Svg.Styled.Attributes as Svg exposing (css)
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.GraphComponentsAggregatedTracing as Theme
import Util.TextDimensions
import View.Locale as Locale
import View.Pathfinder.Tx.Utils exposing (Pos, toPosition)



-- view : Plugins -> View.Config -> Pathfinder.Config -> ExternalConversion -> Address -> Address-> Svg Msg
-- view plugins vc gc tx conversion =
--     Svg.g [] []


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


view : View.Config -> Conversion -> Address -> Address -> Maybe (Svg Msg) -> Svg Msg
view vc conversion inputAddress outputAddress maybeIcon =
    let
        cr =
            conversion.raw

        -- Length of horizontal extension from nodes
        labelText =
            case cr.conversionType of
                Api.Data.ExternalConversionConversionTypeDexSwap ->
                    Locale.string vc.locale "Swap" ++ " " ++ conversion.fromAsset ++ " / " ++ conversion.toAsset

                Api.Data.ExternalConversionConversionTypeBridgeTx ->
                    Locale.string vc.locale "Bridge Tx" ++ " " ++ cr.fromNetwork ++ "." ++ conversion.fromAsset ++ " / " ++ cr.toNetwork ++ "." ++ conversion.toAsset

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
        nodeRadius =
            5

        iconSize =
            nodeRadius * 0.8

        labelOffset =
            nodeRadius + (Util.TextDimensions.estimateTextWidth vc.characterDimensions labelText / 2) + 10

        -- Create simple curved path using single cubic Bézier
        pat =
            pathD
                [ M ( startX, startY ) -- Start at node
                , C ( control1X, control1Y ) ( control2X, control2Y ) ( endX, endY ) -- Single curve
                ]

        hl =
            conversion.hovered

        -- Create the circular node
        circularNode =
            Svg.Styled.circle
                [ Svg.cx (String.fromFloat nodeX)
                , Svg.cy (String.fromFloat nodeY)
                , Svg.r (String.fromFloat nodeRadius)
                , css
                    [ Css.property "fill" Colors.black0
                    ]
                ]
                []

        -- Icon in the center of the node
        iconElement =
            case maybeIcon of
                Just icon ->
                    Svg.Styled.g
                        [ Svg.transform
                            ("translate("
                                ++ String.fromFloat (nodeX - iconSize / 2)
                                ++ ","
                                ++ String.fromFloat (nodeY - iconSize / 2)
                                ++ ") scale("
                                ++ String.fromFloat (iconSize / 16)
                                ++ ")"
                            )
                        ]
                        [ icon ]

                Nothing ->
                    Svg.Styled.g [] []

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
        , circularNode

        -- Icon in center
        , iconElement

        -- Text label
        , textLabel
        ]



-- Keep the original edge function for backward compatibility with default curvature


edge : View.Config -> Conversion -> Address -> Address -> Svg Msg
edge vc conversion inputAddress outputAddress =
    view vc conversion inputAddress outputAddress Nothing
