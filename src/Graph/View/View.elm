module Graph.View.View exposing (view)

import Graph.Css as Css
import Graph.Model exposing (..)
import Graph.Msg exposing (Msg(..))
import Graph.View.Config as Config
import Graph.View.Graph as Graph
import Graph.View.Navbar as Navbar
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import View.Config exposing (Config)


view : Config -> Model -> Html Msg
view vc model =
    section
        [ Css.root vc |> css
        ]
        [ Navbar.navbar vc
        , Graph.graph vc Config.default model
        ]
