module View.Graph.View exposing (view)

import Config.Graph as Config
import Config.View exposing (Config)
import Css.Graph as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Graph exposing (..)
import Msg.Graph exposing (Msg(..))
import View.Graph.Graph as Graph
import View.Graph.Navbar as Navbar


view : Config -> Model -> Html Msg
view vc model =
    section
        [ Css.root vc |> css
        ]
        [ Navbar.navbar vc
        , Graph.graph vc Config.default model
        ]
