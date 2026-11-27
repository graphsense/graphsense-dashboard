module View.Pathfinder.ConversionEdge exposing (Curve(..), Layout, assetsLabel, forwardLayout, isSideBySide, layout, restingIcon, rowLegLabel, view, withDisplacementIndex)

import Api.Data
import Config.View as View
import Css
import Dict exposing (Dict)
import Html.Styled.Events exposing (onMouseLeave)
import List.Extra
import Model.Currency as Currency
import Model.Locale as Locale
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Id exposing (Id)
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


{-| Whether the input side (the input leg's address) stands left of the output
side, as a bridge laid out left to right puts them (in grid units). Ends above
one another (a swap's U-turn, a bridge drawn from its row) or the other way
round keep the curve bulging out to the right.
-}
isSideBySide : Pos -> Pos -> Bool
isSideBySide input output =
    output.x - input.x >= 2


{-| Icon position and curve for an edge between two nodes side by side: from
the right of the left node to the left of the right node, the icon halfway.
Several edges between the same nodes bow apart vertically. A dragged icon stays
on the curve, as in `layout`.
-}
forwardLayout :
    { start : ( Float, Float )
    , end : ( Float, Float )
    , displacementIndex : Int
    , nodeOffset : { x : Float, y : Float }
    }
    -> Layout
forwardLayout { start, end, displacementIndex, nodeOffset } =
    let
        ( startX, startY ) =
            start

        ( endX, endY ) =
            end

        reach =
            max 20 ((endX - startX) / 3)

        bow =
            40 * toFloat displacementIndex

        shift k ( x, y ) =
            ( x + k * nodeOffset.x, y + k * nodeOffset.y )

        ( c1X, c1Y ) =
            shift (4 / 3) ( startX + reach, startY + bow )

        ( c2X, c2Y ) =
            shift (4 / 3) ( endX - reach, endY + bow )
    in
    { curve = Bezier ( c1X, c1Y ) ( c2X, c2Y )
    , node =
        ( (startX + 3 * c1X + 3 * c2X + endX) / 8
        , (startY + 3 * c1Y + 3 * c2Y + endY) / 8
        )
    }


{-| Where an edge between `inputAddress` and `outputAddress` runs and where its
icon sits, for an icon moved by `offset`.
-}
geometry :
    Int
    -> { x : Float, y : Float }
    -> Address
    -> Address
    ->
        { left : Pos
        , right : Pos
        , sideBySide : Bool
        , startX : Float
        , startY : Float
        , endPoint : ( Float, Float )
        , edgeLayout : Layout
        }
geometry displacementIndex offset inputAddress outputAddress =
    let
        aPos =
            toPosition inputAddress

        bPos =
            toPosition outputAddress

        ( left, right ) =
            if aPos.x < bPos.x then
                ( aPos, bPos )

            else
                ( bPos, aPos )

        fd =
            GraphComponents.addressNodeNodeFrame_details

        rad =
            fd.width / 2 + fd.strokeWidth

        -- only when the money flows left to right: an input side right of
        -- the output side (legs not laid out by the dashboard) keeps the curve
        sideBySide =
            isSideBySide aPos bPos

        -- Attach to the right side of both nodes, or, side by side, from the
        -- right of the left one to the left of the right one
        startX =
            left.x * unit + rad

        startY =
            left.y * unit

        endPoint =
            if sideBySide then
                ( right.x * unit - rad, right.y * unit )

            else
                ( right.x * unit + rad, right.y * unit )
    in
    { left = left
    , right = right
    , sideBySide = sideBySide
    , startX = startX
    , startY = startY
    , endPoint = endPoint
    , edgeLayout =
        (if sideBySide then
            forwardLayout

         else
            layout
        )
            { start = ( startX, startY )
            , end = endPoint
            , displacementIndex = displacementIndex
            , nodeOffset = offset
            }
    }


{-| Where the icon of `conversion` sits before it is moved, in graph pixels;
Nothing when an end is not on the graph.
-}
restingIcon : Dict ( Id, Id ) ConversionEdge -> ( Id, Id ) -> Maybe ( Float, Float )
restingIcon conversions key =
    Dict.values conversions
        |> withDisplacementIndex
        |> List.Extra.find (\( _, ( c, _, _ ) ) -> c.id == key)
        |> Maybe.map (\( i, ( _, a, b ) ) -> (geometry i { x = 0, y = 0 } a b).edgeLayout.node)


{-| The conversions with both ends on the graph, each with its index among
those between the same two addresses: several bow apart.
-}
withDisplacementIndex : List ConversionEdge -> List ( Int, ( ConversionEdge, Address, Address ) )
withDisplacementIndex =
    List.filterMap (\x -> Maybe.map2 (\a b -> ( x, a, b )) x.inputAddress x.outputAddress)
        >> List.Extra.gatherEqualsBy (\( _, a, b ) -> ( a, b ))
        >> List.concatMap (\( first, rest ) -> first :: rest |> List.indexedMap Tuple.pair)


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
            if ConversionEdge.isRowOnly conversion then
                rowLegLabel locale cr.fromNetwork cr.fromAsset cr.fromIsSupportedAsset
                    ++ " / "
                    ++ rowLegLabel locale cr.toNetwork cr.toAsset cr.toIsSupportedAsset

            else
                bridgeLegLabel cr.fromNetwork cr.fromAsset conversion.fromAsset
                    ++ " / "
                    ++ bridgeLegLabel cr.toNetwork cr.toAsset conversion.toAsset


{-| A leg of a bridge drawn from its row, as `NETWORK-TICKER` like any bridge
leg. A token outside the registry is named by its short contract and never by a
symbol: it has no ticker the app could vouch for.
-}
rowLegLabel : Locale.Model -> String -> String -> Bool -> String
rowLegLabel locale network asset supported =
    String.toUpper network
        ++ "-"
        ++ (if supported then
                legLabel locale network asset Nothing

            else
                truncateLongIdentifierWithLengths 8 4 asset
           )


{-| The marker below the assets of a bridge drawn from its row.
-}
unsupportedLabel : Locale.Model -> ConversionEdge -> Maybe String
unsupportedLabel locale conversion =
    let
        cr =
            conversion.raw
    in
    if not (ConversionEdge.isRowOnly conversion) then
        Nothing

    else if not cr.fromIsSupportedAsset && not cr.toIsSupportedAsset then
        Just (Locale.string locale "Unsupported tokens")

    else
        Just (Locale.string locale "Unsupported token")


{-| A bridge leg as `NETWORK-ASSET`. A native leg is the network's gas coin
(arb pays in ETH), not the currency of its loaded tx, which for a native
transfer is the network code. Token legs keep the tx's asset.
-}
bridgeLegLabel : String -> String -> String -> String
bridgeLegLabel network rawAsset txAsset =
    let
        asset =
            if rawAsset == ConversionEdge.nativeAsset then
                (Currency.assetFromBase network).asset

            else
                txAsset
    in
    String.toUpper network ++ "-" ++ String.toUpper asset


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

        labelTextLine3 =
            unsupportedLabel vc.locale conversion

        -- a third line moves the two above it up, centering all three
        lineShift =
            if labelTextLine3 == Nothing then
                0

            else
                7

        { left, right, sideBySide, startX, startY, endPoint, edgeLayout } =
            geometry displacementIndex (conversion.nodeOffset |> Maybe.withDefault { x = 0, y = 0 }) inputAddress outputAddress

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

        labelOffsetLine3 =
            labelTextLine3
                |> Maybe.map (\t -> iconSize + (Util.TextDimensions.estimateTextWidth vc.characterDimensions t / 2) + 2)
                |> Maybe.withDefault 0

        lableOffset =
            max labelOffsetLine1 labelOffsetLine2 |> max labelOffsetLine3

        -- Right of the icon, or, on an edge between nodes side by side, above
        -- it: right of it the text would sit on the edge, below it among the
        -- address labels. Each line's position.
        -- below it instead when the right end is the higher one: the icon
        -- then sits just under that end, among its address label
        ( labelX, labelY ) =
            if sideBySide && right.y < left.y - 0.5 then
                ( nodeX
                , \line -> nodeY + iconSize / 2 + 8 + 14 * toFloat (line - 1)
                )

            else if sideBySide then
                ( nodeX
                , \line -> nodeY - iconSize / 2 - 6 - 14 * toFloat (labelLines - line)
                )

            else
                ( nodeX + lableOffset
                , \line -> nodeY - 7 - lineShift + 14 * toFloat (line - 1)
                )

        labelLines =
            if labelTextLine3 == Nothing then
                2

            else
                3

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
                        [ Svg.x (String.fromFloat labelX)
                        , Svg.y (String.fromFloat (labelY 1))
                        , Svg.textAnchor "middle"
                        , Svg.dominantBaseline "middle"
                        , css
                            [ Css.fontWeight (Css.int 600)
                            ]
                        ]
                        [ Svg.Styled.text labelTextLine1 ]
                    , Svg.Styled.text_
                        [ Svg.x (String.fromFloat labelX)
                        , Svg.y (String.fromFloat (labelY 2))
                        , Svg.textAnchor "middle"
                        , Svg.dominantBaseline "middle"
                        , css
                            []
                        ]
                        [ Svg.Styled.text labelTextLine2 ]
                    , labelTextLine3
                        |> Maybe.map
                            (\t ->
                                Svg.Styled.text_
                                    [ Svg.x (String.fromFloat labelX)
                                    , Svg.y (String.fromFloat (labelY 3))
                                    , Svg.textAnchor "middle"
                                    , Svg.dominantBaseline "middle"
                                    , css [ Css.fontStyle Css.italic ]
                                    , testId "gs-bridge-unsupported-label"
                                    ]
                                    [ Svg.Styled.text t ]
                            )
                        |> Maybe.withDefault (Svg.Styled.g [] [])
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
