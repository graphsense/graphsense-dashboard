module View.Graph exposing (view)

import Conditional exposing (applyIf)
import Config.Graph as Graph
import Config.View exposing (Config)
import Css.Graph as Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Html exposing (..)
import IntDict exposing (IntDict)
import Json.Decode
import List.Extra
import Log
import Model.Graph exposing (..)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Id as Id
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Graph.Transform as Transform
import Msg.Graph exposing (Msg(..))
import Plugin as Plugin exposing (Plugins)
import RecordSetter exposing (..)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Tuple exposing (..)
import Util.Graph as Util
import Util.View exposing (none)
import View.Graph.Address as Address
import View.Graph.Browser exposing (browser)
import View.Graph.Entity as Entity
import View.Graph.Layer as ViewLayer
import View.Graph.Link as Link
import View.Graph.Navbar as Navbar
import View.Graph.Transform as Transform


view : Plugins -> Config -> Model -> Html Msg
view plugins vc model =
    section
        [ Css.root vc |> Html.css
        ]
        [ Navbar.navbar plugins vc model
        , graph plugins vc model.config model
        ]


graph : Plugins -> Config -> Graph.Config -> Model -> Html Msg
graph plugins vc gc model =
    Html.section
        [ Css.graphRoot vc |> Html.css
        , Html.id "graph"
        ]
        [ browser plugins vc gc model.plugins model.browser
        , model.size
            |> Maybe.map (graphSvg plugins vc gc model)
            |> Maybe.withDefault none
        ]


graphSvg : Plugins -> Config -> Graph.Config -> Model -> Coords -> Svg Msg
graphSvg plugins vc gc model size =
    let
        dim =
            { width = size.x, height = size.y }
    in
    svg
        ([ preserveAspectRatio "xMidYMid meet"
         , Transform.viewBox dim model.transform |> viewBox
         , Css.svgRoot vc |> Svg.css
         , UserClickedGraph
            |> onClick
         , Svg.custom "wheel"
            (Json.Decode.map3
                (\y mx my ->
                    { message = UserWheeledOnGraph mx my y
                    , stopPropagation = False
                    , preventDefault = False
                    }
                )
                (Json.Decode.field "deltaY" Json.Decode.float)
                (Json.Decode.field "offsetX" Json.Decode.float)
                (Json.Decode.field "offsetY" Json.Decode.float)
            )
         , Svg.on "mousedown"
            (Util.decodeCoords Coords
                |> Json.Decode.map UserPushesLeftMouseButtonOnGraph
            )
         ]
            ++ (if model.dragging /= NoDragging then
                    Svg.preventDefaultOn "mousemove"
                        (Util.decodeCoords Coords
                            |> Json.Decode.map (\c -> ( UserMovesMouseOnGraph c, True ))
                        )
                        |> List.singleton

                else
                    []
               )
        )
        (let
            -- avoid Maybe EntityId/AddressId to make the selected
            -- value work with lazy
            ( selectedEntity, selectedAddress ) =
                case model.selected of
                    SelectedEntity id ->
                        ( id, Id.noAddressId )

                    SelectedAddress id ->
                        ( Id.noEntityId, id )

                    SelectedNone ->
                        ( Id.noEntityId, Id.noAddressId )

            ( hoveredEntityLink, hoveredAddressLink ) =
                case model.hovered of
                    HoveredEntityLink id ->
                        ( id, Id.noAddressLinkId )

                    HoveredAddressLink id ->
                        ( Id.noEntityLinkId, id )

                    HoveredNone ->
                        ( Id.noEntityLinkId, Id.noAddressLinkId )
         in
         [ Svg.lazy2 arrowMarkers vc gc
         , Svg.lazy3 entityLinks vc gc model.layers
         , Svg.lazy4 entities vc gc selectedEntity model.layers
         , Svg.lazy3 addressLinks vc gc model.layers
         , Svg.lazy5 addresses plugins vc gc selectedAddress model.layers
         , Svg.lazy5 hoveredLink vc gc hoveredEntityLink hoveredAddressLink model.layers
         ]
        )


addresses : Plugins -> Config -> Graph.Config -> Id.AddressId -> IntDict Layer -> Svg Msg
addresses plugins vc gc selected layers =
    let
        _ =
            Log.log "Graph.addresses" ""
    in
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "layer" ++ String.fromInt layerId
                , Svg.lazy5 ViewLayer.addresses plugins vc gc selected layer
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entities : Config -> Graph.Config -> Id.EntityId -> IntDict Layer -> Svg Msg
entities vc gc selected layers =
    let
        _ =
            Log.log "Graph.entities" ""
    in
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "layer" ++ String.fromInt layerId
                , Svg.lazy4 ViewLayer.entities vc gc selected layer
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


entityLinks : Config -> Graph.Config -> IntDict Layer -> Svg Msg
entityLinks vc gc layers =
    let
        _ =
            Log.log "Graph.entityLinks" ""
    in
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "entityLinks" ++ String.fromInt layerId
                , Svg.lazy3 ViewLayer.entityLinks vc gc layer
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


addressLinks : Config -> Graph.Config -> IntDict Layer -> Svg Msg
addressLinks vc gc layers =
    let
        _ =
            Log.log "Graph.addressLinks" ""
    in
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "addressLinks" ++ String.fromInt layerId
                , Svg.lazy3 ViewLayer.addressLinks vc gc layer
                )
                    :: svg
            )
            []
        |> Keyed.node "g" []


hoveredLink : Config -> Graph.Config -> Id.LinkId Id.EntityId -> Id.LinkId Id.AddressId -> IntDict Layer -> Svg Msg
hoveredLink vc gc hoveredEntityLink hoveredAddressLink layers =
    let
        el =
            if hoveredEntityLink /= Id.noEntityLinkId then
                Id.getSourceId hoveredEntityLink
                    |> Id.layer
                    |> (\l -> IntDict.get l layers)
                    |> Maybe.map (ViewLayer.calcRange vc gc)
                    |> Maybe.andThen
                        (\( mn, mx ) ->
                            Layer.getEntityLink hoveredEntityLink layers
                                |> Maybe.map
                                    (\( source, target ) ->
                                        Svg.lazy6 Link.entityLinkHovered vc gc mn mx source target
                                    )
                        )

            else if hoveredAddressLink /= Id.noAddressLinkId then
                Id.getSourceId hoveredAddressLink
                    |> Id.layer
                    |> (\l -> IntDict.get l layers)
                    |> Maybe.map (ViewLayer.calcAddressRange vc gc)
                    |> Maybe.andThen
                        (\( mn, mx ) ->
                            Layer.getAddressLink hoveredAddressLink layers
                                |> Maybe.map
                                    (\( source, target ) ->
                                        Svg.lazy6 Link.addressLinkHovered vc gc mn mx source target
                                    )
                        )

            else
                Nothing
    in
    el
        |> Maybe.withDefault none


arrowMarkers : Config -> Graph.Config -> Svg Msg
arrowMarkers vc gc =
    [ vc.theme.graph.linkColorFaded
    , vc.theme.graph.linkColorStrong
    , vc.theme.graph.linkColorSelected
    ]
        |> List.map (Link.arrowMarker vc gc)
        |> defs []
