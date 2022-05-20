module Plugin.View.Graph.Address exposing (..)

import Dict exposing (Dict)
import Json.Decode
import Model.Graph.Address exposing (Address)
import Msg.Graph exposing (Msg(..))
import Plugin exposing (Plugin)
import Svg exposing (Svg)


flags : Dict String Plugin -> Address -> List (Svg Msg)
flags plugins address =
    plugins
        |> Dict.toList
        |> List.filterMap
            (\( pid, plugin ) ->
                Dict.get pid address.plugins
                    |> Maybe.map
                        (\state ->
                            plugin.view.graph.address.flags state address
                                |> Svg.map (PluginMsg pid Plugin.Address)
                        )
            )



{-
   * allow plugins to store state related to things in graph
-}
