module Plugin.View.Graph.Navbar exposing (left)

import Config.View as View
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Model.Graph as Graph
import Msg.Graph exposing (Msg(..))
import Plugin as Plugin exposing (..)
import Plugin.Model as Plugin exposing (..)


left : Plugins -> View.Config -> Graph.Model -> List (Html Msg)
left plugins vc graph =
    plugins
        |> Dict.toList
        |> List.filterMap
            (\( pid, plugin ) ->
                Dict.get pid graph.plugins
                    |> plugin.view.graph.navbar.left vc
                    |> Maybe.map (Html.map (PluginMsg pid Plugin.Model))
            )
