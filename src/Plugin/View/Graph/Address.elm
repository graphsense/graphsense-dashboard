module Plugin.View.Graph.Address exposing (..)

import Config.View as View
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Json.Decode
import Model.Graph exposing (Model)
import Model.Graph.Address exposing (..)
import Model.Graph.ContextMenu exposing (Type(..))
import Msg.Graph exposing (Msg(..))
import Plugin as Plugin exposing (..)
import Plugin.Model as Plugin exposing (..)
import Svg.Styled as Svg exposing (Svg)


flags : Plugins -> View.Config -> Address -> List (Svg Msg)
flags plugins vc address =
    plugins
        |> Dict.toList
        |> List.map
            (\( pid, plugin ) ->
                Dict.get pid address.plugins
                    |> Maybe.map (plugin.view.graph.address.flags vc)
                    |> Maybe.withDefault []
                    |> List.map (Svg.map (PluginMsg pid))
            )
        |> List.concat


properties : Plugins -> PluginStates -> PluginStates -> View.Config -> List (Html Msg)
properties plugins states addressStates vc =
    plugins
        |> Dict.toList
        |> List.map
            (\( pid, plugin ) ->
                Maybe.map2 (plugin.view.graph.address.properties vc)
                    (Dict.get pid states)
                    (Dict.get pid addressStates)
                    |> Maybe.withDefault []
                    |> List.map (Html.map (PluginMsg pid))
            )
        |> List.concat


contextMenu : Plugins -> PluginStates -> View.Config -> Model -> Address -> List (Html Msg)
contextMenu plugins states vc model address =
    plugins
        |> Dict.toList
        |> List.filterMap
            (\( pid, plugin ) ->
                Dict.get pid states
                    |> Maybe.map
                        (\modelState ->
                            Dict.get pid address.plugins
                                |> plugin.view.graph.address.contextMenu vc address.id modelState
                                |> List.map (Html.map (PluginMsg pid))
                        )
            )
        |> List.concat
