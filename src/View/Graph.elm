module View.Graph exposing (view)

import Config.Graph as Graph
import Config.View exposing (Config)
import Css.Graph as Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Html exposing (..)
import Model.Graph exposing (..)
import Model.Graph.Layer as Layer
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
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


graph : Config -> Graph.Config -> Model -> Html Msg
graph vc gc model =
    Html.section
        [ Css.graphRoot vc |> Html.css
        ]
        [ svg
            [ preserveAspectRatio "xMidYMid slice"
            , Css.svgRoot vc |> Svg.css
            ]
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
