module View.Graph exposing (view)

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
import RecordSetter exposing (..)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Util.Graph as Util
import Util.View exposing (none)
import View.Graph.Address as Address
import View.Graph.Browser exposing (browser)
import View.Graph.Entity as Entity
import View.Graph.Layer as ViewLayer
import View.Graph.Link as Link
import View.Graph.Navbar as Navbar
import View.Graph.Transform as Transform


view : Config -> Model -> Html Msg
view vc model =
    section
        [ Css.root vc |> Html.css
        ]
        [ Navbar.navbar vc
        , graph vc model.config model
        ]


graph : Config -> Graph.Config -> Model -> Html Msg
graph vc gc model =
    Html.section
        [ Css.graphRoot vc |> Html.css
        , Html.id "graph"
        ]
        [ browser vc gc model.browser
        , model.size
            |> Maybe.map
                (\size ->
                    let
                        dim =
                            { width = size.x, height = size.y }
                    in
                    svg
                        ([ preserveAspectRatio "xMidYMid meet"
                         , Transform.viewBox dim model.transform |> viewBox
                         , Css.svgRoot vc |> Svg.css
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
                                    Just x ->
                                        case x of
                                            EntityId id ->
                                                ( id, Id.noAddressId )

                                            AddressId id ->
                                                ( Id.noEntityId, id )

                                    Nothing ->
                                        ( Id.noEntityId, Id.noAddressId )
                         in
                         [ Svg.lazy2 arrowMarkers vc gc
                         , Svg.lazy4 entities vc gc selectedEntity model.layers
                         , Svg.lazy4 addresses vc gc selectedAddress model.layers
                         , Svg.lazy3 entityLinks vc gc model.layers
                         ]
                        )
                )
            |> Maybe.withDefault none
        ]


addresses : Config -> Graph.Config -> Id.AddressId -> IntDict Layer -> Svg Msg
addresses vc gc selected layers =
    let
        _ =
            Log.log "Graph.addresses" ""
    in
    layers
        |> IntDict.foldl
            (\layerId layer svg ->
                ( "layer" ++ String.fromInt layerId
                , Svg.lazy4 ViewLayer.addresses vc gc selected layer
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
                Svg.lazy3 ViewLayer.entityLinks vc gc layer
                    :: svg
            )
            []
        |> g []


arrowMarkers : Config -> Graph.Config -> Svg Msg
arrowMarkers vc gc =
    [ vc.theme.graph.linkColorFaded
    , vc.theme.graph.linkColorStrong
    , vc.theme.graph.linkColorSelected
    ]
        |> List.map (Link.arrowMarker vc gc)
        |> defs []
