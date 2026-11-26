module View.Pathfinder.Overview exposing (view)

{-| The overview map, bottom right of the graph (bottom left is where the
notifications appear).

The dots and edges are drawn in a lazy layer that depends on the graph only;
the viewport box on top is the one part that follows every pan and zoom.

Built from existing icons and buttons and the theme colours; the frame moves
to the generated "Overview map" component once the Figma "Graph filters"
frame is approved.

-}

import Animation
import Config.Pathfinder exposing (TracingMode(..))
import Config.View as View
import Css
import Dict
import Html.Styled exposing (Html, div, span, text)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Events exposing (custom, stopPropagationOn)
import Json.Decode as Decode
import Model.Graph.Coords exposing (BBox)
import Model.Graph.Transform as Transform
import Model.Pathfinder as Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Highlighting as Highlighting exposing (Highlighting)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.Overview as Overview exposing (Layout, Visibility(..))
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..), Selection(..))
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.Overview as OverviewMsg
import RecordSetter as Rs
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as SA
import Svg.Styled.Lazy as Lazy
import Theme.Colors as Colors
import Theme.Html.Buttons as Buttons
import Theme.Html.Icons as Icons
import Util.View exposing (onClickWithStop, testId)
import View.Button as Button
import View.Locale as Locale


{-| Margin around the graph on the map, in graph pixels.
-}
margin : Float
margin =
    unit


{-| `dim`: the size of the graph pane.
-}
view : View.Config -> Pathfinder.Model -> BBox -> Html Msg
view vc model dim =
    let
        points =
            visibleAddresses model.highlighting model.network
                |> List.map (Tuple.second >> position)

        current =
            Transform.getCurrent model.transform

        viewport =
            Transform.coordsToBBox dim current

        inset =
            20 * Transform.getZ model.transform
    in
    if List.isEmpty points then
        text ""

    else if model.overview.visibility == Hidden then
        div [ css panelPosition ]
            [ Button.defaultConfig
                |> Rs.s_text (Locale.string vc.locale "Show overview")
                |> Rs.s_onClick (Just (OverviewMsg OverviewMsg.UserClickedShowOverview))
                |> Rs.s_onClickWithStop True
                |> Rs.s_style Buttons.ButtonStyleOutlined
                |> Rs.s_size Buttons.ButtonSizeSmall
                |> Button.buttonWithAttributes { root = [ testId "gs-overview-show" ], button = [] } vc
            ]

    else if Overview.isShown inset model.overview viewport points then
        Overview.layout margin points
            |> Maybe.map (panel vc model viewport)
            |> Maybe.withDefault (text "")

    else
        text ""


panelPosition : List Css.Style
panelPosition =
    [ Css.position Css.absolute
    , Css.right (Css.px 16)
    , Css.bottom (Css.px 24)
    , Css.property "pointer-events" "all"
    ]


panel : View.Config -> Pathfinder.Model -> BBox -> Layout -> Html Msg
panel vc model viewport layout =
    let
        toGraph =
            Decode.map2 (\x y -> Overview.toGraph layout { x = x, y = y })
                (Decode.field "offsetX" Decode.float)
                (Decode.field "offsetY" Decode.float)

        on event toMsg =
            custom event
                (Decode.map
                    (\p -> { message = OverviewMsg (toMsg p), stopPropagation = True, preventDefault = True })
                    toGraph
                )

        release event =
            stopPropagationOn event (Decode.succeed ( OverviewMsg OverviewMsg.UserReleasedOverview, True ))

        topLeft =
            Overview.toMap layout { x = viewport.x, y = viewport.y }

        bottomRight =
            Overview.toMap layout { x = viewport.x + viewport.width, y = viewport.y + viewport.height }
    in
    div
        [ testId "gs-overview"
        , css
            (panelPosition
                ++ [ Css.width (Css.px Overview.mapWidth)
                   , Css.borderRadius (Css.px 6)
                   , Css.overflow Css.hidden
                   , Css.border3 (Css.px 1) Css.solid Css.transparent
                   , Css.property "border-color" Colors.greyBlue100
                   , Css.property "background-color" Colors.white
                   , Css.property "box-shadow" ("0 2px 8px " ++ Colors.greyShadow)
                   ]
            )
        ]
        [ div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.padding3 (Css.px 3) (Css.px 6) (Css.px 3)
                , Css.fontSize (Css.px 11)
                , Css.property "color" Colors.greyBlue700
                , Css.borderBottom3 (Css.px 1) Css.solid Css.transparent
                , Css.property "border-bottom-color" Colors.greyBlue50
                ]
            ]
            [ span [ css [ Css.flexGrow (Css.num 1) ] ] [ Locale.text vc.locale "Overview" ]
            , span
                [ testId "gs-overview-hide"
                , HA.title (Locale.string vc.locale "Hide overview")
                , css [ Css.cursor Css.pointer, Css.displayFlex ]
                , onClickWithStop (OverviewMsg OverviewMsg.UserClickedHideOverview)
                ]
                [ Icons.iconsMinusSnoPadding {} ]
            ]
        , div
            ([ testId "gs-overview-map"
             , css
                [ Css.position Css.relative
                , Css.width (Css.px Overview.mapWidth)
                , Css.height (Css.px Overview.mapHeight)
                , Css.cursor Css.crosshair
                ]
             , on "mousedown" OverviewMsg.UserPressedOverview
             , release "mouseup"
             , release "mouseleave"
             ]
                ++ (if model.overview.dragging then
                        [ on "mousemove" OverviewMsg.UserDraggedOverview ]

                    else
                        []
                   )
            )
            [ Svg.svg
                [ SA.width (String.fromFloat Overview.mapWidth)
                , SA.height (String.fromFloat Overview.mapHeight)
                , SA.css [ Css.display Css.block, Css.property "pointer-events" "none" ]
                ]
                [ Lazy.lazy4 graphLayer model.config.tracingMode model.highlighting model.selection model.network
                , Svg.rect
                    [ SA.x (String.fromFloat topLeft.x)
                    , SA.y (String.fromFloat topLeft.y)
                    , SA.width (String.fromFloat (max 2 (bottomRight.x - topLeft.x)))
                    , SA.height (String.fromFloat (max 2 (bottomRight.y - topLeft.y)))
                    , SA.css
                        [ Css.property "fill" Colors.blue400
                        , Css.property "fill-opacity" "0.1"
                        , Css.property "stroke" Colors.blue400
                        , Css.property "stroke-width" "1.2"
                        ]
                    , testId "gs-overview-viewport"
                    ]
                    []
                ]
            ]
        ]


{-| Edges first, then dots: starting points red, the selection blue.
-}
graphLayer : TracingMode -> Highlighting -> Selection -> Network -> Svg Msg
graphLayer mode highlighting selection network =
    let
        addresses =
            visibleAddresses highlighting network

        points =
            List.map (Tuple.second >> position) addresses
    in
    case Overview.layout margin points of
        Nothing ->
            Svg.g [] []

        Just layout ->
            let
                at =
                    Overview.toMap layout

                selected =
                    selectedIds selection

                line a b =
                    let
                        p =
                            at a

                        q =
                            at b
                    in
                    Svg.line
                        [ SA.x1 (String.fromFloat p.x)
                        , SA.y1 (String.fromFloat p.y)
                        , SA.x2 (String.fromFloat q.x)
                        , SA.y2 (String.fromFloat q.y)
                        , SA.css [ Css.property "stroke" Colors.greyBlue200, Css.property "stroke-width" "0.6" ]
                        ]
                        []

                addressPos id =
                    if Highlighting.forNode highlighting id == Nothing then
                        Nothing

                    else
                        Dict.get id network.addresses |> Maybe.map position

                edges =
                    case mode of
                        TransactionTracingMode ->
                            network.txs
                                |> Dict.values
                                |> List.filter (\tx -> Highlighting.forNode highlighting tx.id /= Nothing)
                                |> List.concatMap
                                    (\tx ->
                                        Tx.listAddressesForTx tx
                                            |> List.filterMap (Tuple.second >> .id >> addressPos)
                                            |> List.map (line (txPosition tx))
                                    )

                        AggregateTracingMode ->
                            network.aggEdges
                                |> Dict.values
                                |> List.filter (\e -> Highlighting.forAggEdge highlighting ( e.a, e.b ) /= Nothing)
                                |> List.filterMap (\e -> Maybe.map2 line (addressPos e.a) (addressPos e.b))

                dot ( id, address ) =
                    let
                        p =
                            at (position address)
                    in
                    Svg.circle
                        [ SA.cx (String.fromFloat p.x)
                        , SA.cy (String.fromFloat p.y)
                        , SA.r "2"
                        , SA.css
                            [ Css.property "fill" <|
                                if List.member id selected then
                                    Colors.blue400

                                else if address.isStartingPoint then
                                    Colors.red500

                                else
                                    Colors.greyBlue700
                            ]
                        ]
                        []
            in
            Svg.g [] (edges ++ List.map dot addresses)


visibleAddresses : Highlighting -> Network -> List ( Id, Address )
visibleAddresses highlighting network =
    network.addresses
        |> Dict.toList
        |> List.filter (\( id, _ ) -> Highlighting.forNode highlighting id /= Nothing)


{-| Resting position in graph pixels, so the map does not move while nodes
animate.
-}
position : Address -> { x : Float, y : Float }
position a =
    { x = (a.x + a.dx) * unit, y = (Animation.getTo a.y + a.dy) * unit }


txPosition : Tx -> { x : Float, y : Float }
txPosition t =
    { x = (t.x + t.dx) * unit, y = (Animation.getTo t.y + t.dy) * unit }


selectedIds : Selection -> List Id
selectedIds selection =
    case selection of
        SelectedAddress id ->
            [ id ]

        MultiSelect opts ->
            List.filterMap
                (\o ->
                    case o of
                        MSelectedAddress id ->
                            Just id

                        MSelectedTx _ ->
                            Nothing
                )
                opts

        _ ->
            []
