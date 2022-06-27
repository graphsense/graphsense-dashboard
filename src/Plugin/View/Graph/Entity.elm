module Plugin.View.Graph.Entity exposing (..)

import Config.View as View
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Json.Decode
import Model.Graph exposing (Model)
import Model.Graph.ContextMenu exposing (Type(..))
import Model.Graph.Entity exposing (..)
import Msg.Graph exposing (Msg(..))
import Plugin as Plugin exposing (..)
import Plugin.Model as Plugin exposing (..)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as Svg
import Tuple exposing (..)
import Util.Graph


flags : Plugins -> View.Config -> Float -> Entity -> List (Svg Msg)
flags plugins vc offset entity =
    plugins
        |> Dict.toList
        |> List.foldl
            (\( pid, plugin ) ( off, otherFlags ) ->
                Dict.get pid entity.plugins
                    |> Maybe.map
                        (plugin.view.graph.entity.flags vc)
                    |> Maybe.map
                        (\( pOff, pFlags ) ->
                            let
                                newOff =
                                    off + vc.theme.graph.flagsGap
                            in
                            pFlags
                                |> List.map (Svg.map (PluginMsg pid))
                                |> Svg.g
                                    [ Util.Graph.translate -newOff 0
                                        |> Svg.transform
                                    ]
                                |> (\f -> f :: otherFlags)
                                |> pair (newOff + pOff)
                        )
                    |> Maybe.withDefault ( off, otherFlags )
            )
            ( offset, [] )
        |> second


properties : Plugins -> PluginStates -> PluginStates -> View.Config -> List (Html Msg)
properties plugins states entityStates vc =
    plugins
        |> Dict.toList
        |> List.map
            (\( pid, plugin ) ->
                Maybe.map2 (plugin.view.graph.entity.properties vc)
                    (Dict.get pid states)
                    (Dict.get pid entityStates)
                    |> Maybe.withDefault []
                    |> List.map (Html.map (PluginMsg pid))
            )
        |> List.concat


contextMenu : Plugins -> PluginStates -> View.Config -> Model -> Entity -> List (Html Msg)
contextMenu plugins states vc model entity =
    plugins
        |> Dict.toList
        |> List.filterMap
            (\( pid, plugin ) ->
                Dict.get pid states
                    |> Maybe.map
                        (\modelState ->
                            Dict.get pid entity.plugins
                                |> plugin.view.graph.entity.contextMenu vc entity.id modelState
                                |> List.map (Html.map (PluginMsg pid))
                        )
            )
        |> List.concat
