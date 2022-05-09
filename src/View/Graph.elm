module View.Graph exposing (view)

import Config.Graph as Graph
import Config.View exposing (Config)
import Css.Graph as Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Html exposing (..)
import Json.Decode
import Model.Graph exposing (..)
import Model.Graph.Layer as Layer
import Model.Graph.Transform as Transform
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Update.Graph.Transform as Transform
import View.Graph.Address as Address
import View.Graph.Entity as Entity
import View.Graph.Navbar as Navbar


view : Config -> Model -> Html Msg
view vc model =
    section
        [ Css.root vc |> Html.css
        ]
        [ Navbar.navbar vc
        , graph vc (Graph.default |> s_colors model.colors) model
        ]


decodeCoords : Json.Decode.Decoder Transform.Coords
decodeCoords =
    Json.Decode.map2 Transform.Coords
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)


graph : Config -> Graph.Config -> Model -> Html Msg
graph vc gc model =
    Html.section
        [ Css.graphRoot vc |> Html.css
        ]
        [ svg
            ([ preserveAspectRatio "xMidYMid slice"
             , Svg.id "graph"
             , let
                transform =
                    Transform.get model.transform
               in
               [ transform.x
               , transform.y
               , model.width
               , model.height
               ]
                |> List.map String.fromFloat
                |> List.intersperse " "
                |> String.concat
                |> viewBox
             , Css.svgRoot vc |> Svg.css
             , Svg.on "mousedown"
                (decodeCoords
                    |> Json.Decode.map UserPushesLeftMouseButtonOnGraph
                )
             ]
                ++ (if model.transform.dragging /= Transform.NoDragging then
                        [ Svg.preventDefaultOn "mousemove"
                            (decodeCoords
                                |> Json.Decode.map (\c -> ( UserMovesMouseOnGraph c, True ))
                            )
                        ]

                    else
                        []
                   )
            )
            [ entities vc gc model
            , addresses vc gc model
            ]
        ]


addresses : Config -> Graph.Config -> Model -> Svg Msg
addresses vc gc model =
    model.layers
        |> Layer.addresses
        |> List.map (Address.address vc gc)
        |> g []


entities : Config -> Graph.Config -> Model -> Svg Msg
entities vc gc model =
    model.layers
        |> Layer.entities
        |> List.map (Entity.entity vc gc)
        |> g []
