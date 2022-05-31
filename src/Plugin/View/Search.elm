module Plugin.View.Search exposing (..)

import Config.View as View
import Dict
import Html.Styled as Html exposing (Html)
import Msg.Search exposing (Msg(..))
import Plugin as Plugin exposing (..)
import Plugin.Model as Plugin exposing (..)


placeholder : Plugins -> View.Config -> List String
placeholder plugins vc =
    plugins
        |> Dict.toList
        |> List.map
            (\( pid, plugin ) ->
                plugin.view.search.placeholder vc
            )
        |> List.concat


resultList : Plugins -> PluginStates -> View.Config -> List (Html Msg)
resultList plugins states vc =
    plugins
        |> Dict.toList
        |> List.map
            (\( pid, plugin ) ->
                Dict.get pid states
                    |> Maybe.map (plugin.view.search.resultList vc)
                    |> Maybe.withDefault []
                    |> List.map (Html.map (PluginMsg pid))
            )
        |> List.concat
