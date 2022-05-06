module Graph.View.Graph exposing (graph)

import Graph.Css as Css
import Graph.Model exposing (..)
import Graph.Model.Layer as Layer
import Graph.Msg exposing (Msg(..))
import Graph.View.Address as Address
import Graph.View.Config as Graph
import Graph.View.Navbar as Navbar
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)
import View.Config exposing (Config)


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
