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


propertyBox : Plugins -> View.Config -> String -> PluginStates -> Maybe (Html Msg)
propertyBox plugins vc pid states =
    Maybe.Extra.andThen2 (\plugin -> plugin.view.graph.browser vc)
        (Dict.get pid plugins)
        (Dict.get pid states)
        |> Maybe.map (Html.map (PluginMsg pid))
