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
import Svg.Styled.Attributes as Svg
import Tuple exposing (..)
import Util.Graph


flags : Plugins -> View.Config -> Float -> Address -> List (Svg Msg)
flags plugins vc offset address =
    plugins
        |> Dict.toList
        |> List.foldl
            (\( pid, plugin ) ( off, otherFlags ) ->
                Dict.get pid address.plugins
                    |> Maybe.map
                        (plugin.view.graph.address.flags vc)
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
