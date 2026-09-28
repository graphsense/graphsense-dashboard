module View.Pathfinder.ConversionEdge exposing (Curve(..), Layout, assetsLabel, layout, view)

import Api.Data
import Config.View as View
import Css
import Html.Styled.Events exposing (onMouseLeave)
import Model.Currency as Currency
import Model.Locale as Locale
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.SearchBox exposing (Highlight, dimmedOpacity)
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter as Rs
import Svg.PathD exposing (Segment(..), pathD)
import Svg.Styled exposing (Svg, g, path)
import Svg.Styled.Attributes as Svg exposing (css, filter)
import Svg.Styled.Events exposing (onMouseOver)
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.GraphComponentsAggregatedTracing as Theme
import Util.Graph exposing (mousedown)
import Util.TextDimensions
import Util.View exposing (onClickWithStop, pointer, testId, truncateLongIdentifierWithLengths)
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


{-| The drawn shape of a swap edge: a cubic Bézier between two different
nodes, or a teardrop loop when both ends are the same node.
-}
type Curve
    = Bezier ( Float, Float ) ( Float, Float )
    | Loop
        { c1 : ( Float, Float )
        , c2 : ( Float, Float )
        , tip : ( Float, Float )
        , c3 : ( Float, Float )
        , c4 : ( Float, Float )
        }


type alias Layout =
    { curve : Curve
    , node : ( Float, Float )
    }


{-| Icon position and curve for a swap edge. A dragged icon stays on the curve:
on the Bézier it is the t = 0.5 point (start + 3 c1 + 3 c2 + end) / 8, so
shifting both controls by 4/3 of the offset moves it by exactly the offset; on
a loop it is the tip.
-}
layout :
    { start : ( Float, Float )
    , end : ( Float, Float )
    , displacementIndex : Int
    , nodeOffset : { x : Float, y : Float }
    }
    -> Layout
layout { start, end, displacementIndex, nodeOffset } =
    let
        ( startX, startY ) =
            start

        ( endX, endY ) =
            end

        -- Length of horizontal extension from nodes
        horizontalExtension =
            150.0 + (30 * toFloat displacementIndex)

        -- Teardrop loop parameters
        -- Horizontal extension for teardrop
        loopXDisplacement =
            80.0

        -- Vertical displacement based on displacement index
        loopYDisplacement =
            40.0 + (30 * toFloat displacementIndex)

        shift k ( x, y ) =
            ( x + k * nodeOffset.x, y + k * nodeOffset.y )

        -- Check if start and end points are the same
        isSamePoint =
            abs (startX - endX) < 1.0 && abs (startY - endY) < 1.0
    in
    if isSamePoint then
        -- Create a teardrop-shaped loop with round head
        let
            -- Teardrop tip position
            tip =
                shift 1 ( startX + (loopXDisplacement * 1.2), startY - loopYDisplacement )
        in
        { curve =
            Loop
                { -- Control points for smooth teardrop shape
                  -- Gentle outward curve
                  -- Slight upward
                  c1 = ( startX + (loopXDisplacement * 0.7), startY - (loopYDisplacement * 0.2) )
                , -- Near the tip
                  -- Close to tip height
                  c2 = shift 1 ( startX + (loopXDisplacement * 1.1), startY - (loopYDisplacement * 0.8) )
                , tip = tip
                , -- Return curve control points
                  -- Mirror of c2's x
                  -- Above the tip for round shape
                  c3 = shift 1 ( startX + (loopXDisplacement * 1.1), startY - (loopYDisplacement * 1.2) )
                , -- Gentle return
                  -- Smooth back to start
                  c4 = ( startX + (loopXDisplacement * 0.3), startY - (loopYDisplacement * 0.4) )
                }
        , -- Position node at the tip of the teardrop
          node = tip
        }

    else
        -- Original curve path (unchanged)
        let
            -- Calculate control points for cubic Bézier curve
            -- First control point - extend horizontally to the right from start
            ( c1X, c1Y ) =
                shift (4 / 3) ( startX + horizontalExtension, startY )

            -- Second control point - extend horizontally to the right from end, with curvature offset
            ( c2X, c2Y ) =
                shift (4 / 3) ( endX + horizontalExtension, endY )
        in
        { curve = Bezier ( c1X, c1Y ) ( c2X, c2Y )
        , -- Original calculation for curve (unchanged)
          node =
            ( (startX + 3 * c1X + 3 * c2X + endX) / 8
            , (startY + 3 * c1Y + 3 * c2Y + endY) / 8
            )
        }


{-| Both legs' assets, read from the raw conversion rather than the loaded
nodes: for a same-tx swap one node is the native root.
-}
assetsLabel : Locale.Model -> ConversionEdge -> String
assetsLabel locale conversion =
    let
        cr =
            conversion.raw
    in
    case cr.conversionType of
        Api.Data.ExternalConversionConversionTypeDexSwap ->
            legLabel locale cr.fromNetwork cr.fromAsset cr.fromAssetSymbol
                ++ " / "
                ++ legLabel locale cr.toNetwork cr.toAsset cr.toAssetSymbol

        Api.Data.ExternalConversionConversionTypeBridgeTx ->
            (cr.fromNetwork |> String.toUpper) ++ "-" ++ (conversion.fromAsset |> String.toUpper) ++ " / " ++ (cr.toNetwork |> String.toUpper) ++ "-" ++ (conversion.toAsset |> String.toUpper)


legLabel : Locale.Model -> String -> String -> Maybe String -> String
legLabel locale network asset symbol =
    if asset == ConversionEdge.nativeAsset then
        (Currency.assetFromBase network).asset |> String.toUpper

    else
        Locale.assetTicker locale { network = network, asset = asset } symbol
            |> Maybe.withDefault (truncateLongIdentifierWithLengths 8 4 asset)


view : View.Config -> Highlight -> ConversionEdge -> Int -> Address -> Address -> Svg Msg
view vc searchHighlight conversion displacementIndex inputAddress outputAddress =
    let
        cr =
            conversion.raw

        id =
            conversion.id

        labelTextLine1 =
            case cr.conversionType of
                Api.Data.ExternalConversionConversionTypeDexSwap ->
                    Locale.string vc.locale "Swap"

                Api.Data.ExternalConversionConversionTypeBridgeTx ->
                    Locale.string vc.locale "Bridge TX"

        labelTextLine2 =
            assetsLabel vc.locale conversion

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

        endPoint =
            ( right.x * unit + rad, right.y * unit )

        currentOffset =
            conversion.nodeOffset |> Maybe.withDefault { x = 0, y = 0 }

        edgeLayout =
            layout
                { start = ( startX, startY )
                , end = endPoint
                , displacementIndex = displacementIndex
                , nodeOffset = currentOffset
                }

        -- Create path - either loop or curve
        pat =
            case edgeLayout.curve of
                Loop { c1, c2, tip, c3, c4 } ->
                    pathD
                        [ M ( startX, startY ) -- Start at the node
                        , C c1 c2 tip -- First curve to tip
                        , C c3 c4 ( startX, startY ) -- Return curve to start
                        ]

                Bezier c1 c2 ->
                    pathD
                        [ M ( startX, startY ) -- Start at node
                        , C c1 c2 endPoint -- Single curve
                        ]

        -- Calculate node position
        ( nodeX, nodeY ) =
            edgeLayout.node

        -- Node properties
        iconSize =
            GraphComponents.swapNode_details.width

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
                        , mousedown (UserPushesLeftMouseButtonOnConversionNode id)
                        , onClickWithStop NoOp
                        , testId "gs-swap-node"
                        , css [ Css.cursor Css.move ]
                        ]
                    |> Rs.s_swapNodeInner
                        ([ (Css.property "background-color" <|
                                Colors.greyBlue20
                           )
                            |> Css.important
                         ]
                            |> css
                            |> List.singleton
                        )
                )
                { root = { highlightInvisible = hl } }

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
        ([ UserClickedConversionEdge id conversion
            |> onClickWithStop
         , UserMovesMouseOutConversionEdge id conversion
            |> onMouseLeave
         , UserMovesMouseOverConversionEdge id conversion
            |> onMouseOver
         ]
            ++ dimmedOpacity searchHighlight
        )
        ((if hl then
            [ path
                [ Svg.d pat

                -- , Svg.strokeDasharray "5, 5"
                , css Theme.aggregatedLinkHighlightLine_details.styles
                , pointer
                , css
                    [ Css.property "stroke-width" <| String.fromFloat Theme.aggregatedLinkHighlightLine_details.strokeWidth
                    , Css.property "stroke" <| Colors.pathMiddle
                    , Css.property "opacity" "0.6"
                    , Css.property "fill" "none" |> Css.important
                    , Css.property "stroke-linecap" "round"
                    ]
                , filter "url(#dropShadowEdgeHighlight)"
                ]
                []
            ]

          else
            []
         )
            ++ [ -- Simple curved path or loop
                 path
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
        )



-- Keep the original edge function for backward compatibility with default curvature
