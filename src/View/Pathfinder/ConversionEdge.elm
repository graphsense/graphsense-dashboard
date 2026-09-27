module View.Pathfinder.ConversionEdge exposing (Curve(..), Layout, layout, view)

import Api.Data
import Config.View as View
import Css
import Dict
import Html.Styled.Events exposing (onMouseLeave)
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)
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


{-| Where the curve runs and where the swap icon sits on it. `start`/`end` are
the attachment points (right side of the two nodes), `nodeOffset` is how far
the user dragged the icon from its default place.

A dragged icon keeps the curve running through it: on the Bézier the icon is
the point at t = 0.5, (start + 3 c1 + 3 c2 + end) / 8, so shifting both control
points by 4/3 of the offset moves that point by exactly the offset. On a loop
the icon is the tip, which moves with its two neighbouring control points.

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
        loopXDisplacement =
            80.0

        loopYDisplacement =
            40.0 + (30 * toFloat displacementIndex)

        shift k ( x, y ) =
            ( x + k * nodeOffset.x, y + k * nodeOffset.y )

        isSamePoint =
            abs (startX - endX) < 1.0 && abs (startY - endY) < 1.0
    in
    if isSamePoint then
        let
            tip =
                shift 1 ( startX + (loopXDisplacement * 1.2), startY - loopYDisplacement )
        in
        { curve =
            Loop
                { c1 = ( startX + (loopXDisplacement * 0.7), startY - (loopYDisplacement * 0.2) )
                , c2 = shift 1 ( startX + (loopXDisplacement * 1.1), startY - (loopYDisplacement * 0.8) )
                , tip = tip
                , c3 = shift 1 ( startX + (loopXDisplacement * 1.1), startY - (loopYDisplacement * 1.2) )
                , c4 = ( startX + (loopXDisplacement * 0.3), startY - (loopYDisplacement * 0.4) )
                }
        , node = tip
        }

    else
        let
            ( c1X, c1Y ) =
                shift (4 / 3) ( startX + horizontalExtension, startY )

            ( c2X, c2Y ) =
                shift (4 / 3) ( endX + horizontalExtension, endY )
        in
        { curve = Bezier ( c1X, c1Y ) ( c2X, c2Y )
        , node =
            ( (startX + 3 * c1X + 3 * c2X + endX) / 8
            , (startY + 3 * c1Y + 3 * c2Y + endY) / 8
            )
        }


view : View.Config -> Highlight -> ConversionEdge -> Int -> Address -> Address -> Svg Msg
view vc searchHighlight conversion displacementIndex inputAddress outputAddress =
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

        -- labels from the RAW conversion, not the loaded nodes (for a same-tx
        -- swap one node is the native root): native -> network coin; token ->
        -- the registry ticker matched by contract address, else the curated
        -- symbol the conversion carries
        assetCode network asset symbol =
            if asset == "native" then
                String.toUpper network

            else
                Dict.get network vc.locale.supportedTokens
                    |> Maybe.map .tokenConfigs
                    |> Maybe.withDefault []
                    |> List.filter
                        (\tc ->
                            (tc.contractAddress |> Maybe.map String.toLower)
                                == Just (String.toLower asset)
                        )
                    |> List.head
                    |> Maybe.map .ticker
                    |> (\code ->
                            case code of
                                Just c ->
                                    c

                                Nothing ->
                                    Maybe.withDefault asset symbol
                       )
                    |> String.toUpper

        labelTextLine2 =
            case cr.conversionType of
                Api.Data.ExternalConversionConversionTypeDexSwap ->
                    assetCode cr.fromNetwork cr.fromAsset cr.fromAssetSymbol
                        ++ " / "
                        ++ assetCode cr.toNetwork cr.toAsset cr.toAssetSymbol

                Api.Data.ExternalConversionConversionTypeBridgeTx ->
                    (cr.fromNetwork |> String.toUpper) ++ "-" ++ (conversion.fromAsset |> String.toUpper) ++ " / " ++ (cr.toNetwork |> String.toUpper) ++ "-" ++ (conversion.toAsset |> String.toUpper)

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

        currentOffset =
            conversion.nodeOffset |> Maybe.withDefault { x = 0, y = 0 }

        edgeLayout =
            layout
                { start = ( startX, startY )
                , end = ( right.x * unit + rad, right.y * unit )
                , displacementIndex = displacementIndex
                , nodeOffset = currentOffset
                }

        pat =
            case edgeLayout.curve of
                Loop { c1, c2, tip, c3, c4 } ->
                    pathD
                        [ M ( startX, startY )
                        , C c1 c2 tip
                        , C c3 c4 ( startX, startY )
                        ]

                Bezier c1 c2 ->
                    pathD
                        [ M ( startX, startY )
                        , C c1 c2 ( right.x * unit + rad, right.y * unit )
                        ]

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
                        , mousedown (UserPushesLeftMouseButtonOnConversionNode id currentOffset)

                        -- the icon is grabbed, not clicked: a move cursor says so
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
