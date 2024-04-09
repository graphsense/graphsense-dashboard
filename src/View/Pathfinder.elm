module View.Pathfinder exposing (view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Graph
import Css.Pathfinder as Css
import FontAwesome
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Lazy exposing (..)
import Json.Decode
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (BBox, Coords)
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Network exposing (Network)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import Quicklock exposing (plugin)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Util.Graph
import Util.View exposing (none)
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.Network as Network


view : Plugins -> ModelState -> View.Config -> Model -> { navbar : List (Html Msg), contents : List (Html Msg) }
view plugins states vc model =
    { navbar = []
    , contents = graph plugins states vc {} model
    }


graph : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> List (Html Msg)
graph plugins states vc gc model =
    [ vc.size
        |> Maybe.map (graphSvg plugins states vc gc model)
        |> Maybe.withDefault none
    ]
        ++ [ titleAndSettings plugins states vc gc model
           , graphTools plugins states vc gc model
           , propertiesAndSearch plugins states vc gc model
           ]


boxStyle : List Css.Style
boxStyle =
    []


toolItemStyle : List Css.Style
toolItemStyle =
    [ Css.em 2 |> Css.fontSize
    , Css.px 5 |> Css.marginRight
    , Css.px 5 |> Css.marginLeft
    ]


titleAndSettingsStyle : List Css.Style
titleAndSettingsStyle =
    [ Css.position Css.absolute
    , Css.px 10 |> Css.left
    , Css.px 10 |> Css.top
    ]


graphToolsStyle : List Css.Style
graphToolsStyle =
    [ Css.position Css.absolute
    , Css.pct 50 |> Css.left
    , Css.pct 50 |> Css.right
    , Css.px 40 |> Css.bottom
    , Css.px 200 |> Css.minWidth
    , Css.px 1 |> Css.borderWidth
    , Css.displayFlex
    ]


propertiesAndSearchStyle : List Css.Style
propertiesAndSearchStyle =
    [ Css.position Css.absolute
    , Css.px 10 |> Css.right
    , Css.px 10 |> Css.top
    ]
        ++ boxStyle


titleAndSettings : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
titleAndSettings plugins _ vc gc model =
    div [ titleAndSettingsStyle |> HA.css ]
        [ h1 [] [ Html.Styled.text "Pathfinder" ]
        ]


graphTools : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
graphTools plugins _ vc gc model =
    div
        [ graphToolsStyle |> HA.css
        ]
        [ div [ toolItemStyle |> HA.css ] [ FontAwesome.icon FontAwesome.trash |> Html.fromUnstyled ]
        , div [ toolItemStyle |> HA.css ] [ FontAwesome.icon FontAwesome.redo |> Html.fromUnstyled ]
        , div [ toolItemStyle |> HA.css ] [ FontAwesome.icon FontAwesome.undo |> Html.fromUnstyled ]
        , div [ toolItemStyle |> HA.css ] [ FontAwesome.icon FontAwesome.highlighter |> Html.fromUnstyled ]
        ]


propertiesAndSearch : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
propertiesAndSearch plugins _ vc gc model =
    div [ propertiesAndSearchStyle |> HA.css ]
        [ Html.Styled.text "Pathfinder"
        ]



--propertyBoxTools : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Svg Msg
--propertyBoxTools = div [boxStyle |> HA.css] [Html.Styled.text "Property box tools"]


graphSvg : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> BBox -> Svg Msg
graphSvg plugins _ vc gc model bbox =
    let
        dim =
            { width = bbox.width, height = bbox.height }
    in
    svg
        ([ preserveAspectRatio "xMidYMid meet"
         , Transform.viewBox dim model.transform |> viewBox
         , Css.Graph.svgRoot vc |> Svg.css
         , UserClickedGraph model.dragging
            |> onClick
         , HA.id "graph"
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
            (Util.Graph.decodeCoords Coords
                |> Json.Decode.map UserPushesLeftMouseButtonOnGraph
            )
         ]
            ++ (if model.dragging /= NoDragging then
                    Svg.preventDefaultOn "mousemove"
                        (Util.Graph.decodeCoords Coords
                            |> Json.Decode.map (\c -> ( UserMovesMouseOnGraph c, True ))
                        )
                        |> List.singleton

                else
                    []
               )
        )
        [ model.network |> Maybe.map (Svg.lazy4 networks plugins vc gc) |> Maybe.withDefault (div [] [])
        ]


networks : Plugins -> View.Config -> Pathfinder.Config -> Network -> Svg Msg
networks plugins vc gc network =
    Keyed.node "g"
        []
        [ ( "Pathfinder.network." ++ network.name
          , Svg.lazy4 Network.addresses plugins vc gc network.addresses
          )
        ]
