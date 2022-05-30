module Plugin.View.Graph.Browser exposing (..)

import Config.View as View
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Json.Encode exposing (Value)
import Maybe.Extra
import Model.Graph as Graph
import Model.Graph.Address exposing (..)
import Model.Graph.Browser exposing (..)
import Msg.Graph exposing (Msg(..))
import Plugin as Plugin exposing (..)
import Plugin.Model as Plugin exposing (..)
import Route exposing (toUrl)
import Route.Graph as Route
import Tuple exposing (..)


browser : Plugins -> View.Config -> String -> PluginStates -> List (Html Msg)
browser plugins vc pid states =
    let
        pc =
            { toUrl =
                pair pid
                    >> Route.pluginRoute
                    >> Route.graphRoute
                    >> toUrl
            }
    in
    Maybe.map2 (\plugin -> plugin.view.graph.browser pc vc)
        (Dict.get pid plugins)
        (Dict.get pid states)
        |> Maybe.withDefault []
        |> List.map (Html.map (PluginMsg pid))
