module View.Graph.Graph exposing (graph)

import Config.View exposing (Config)
import Css.Graph as Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Model.Graph exposing (..)
import Model.Graph.Layer as Layer
import Msg.Graph exposing (Msg(..))
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)
import View.Graph.Address as Address
import View.Graph.Config as Graph
import View.Graph.Navbar as Navbar


graph : Config -> Graph.Config -> Model -> Html Msg
graph vc gc model =
    Html.section
        [ Css.graphRoot vc |> Html.css
        ]
        [ svg
            [ preserveAspectRatio "xMidYMid slice"
            ]
            [ addresses vc gc model
            ]
        ]


addresses : Config -> Graph.Config -> Model -> Svg Msg
addresses vc gc model =
    model.layers
        |> Layer.addresses
        |> List.map (Address.address vc gc)
        |> g []
