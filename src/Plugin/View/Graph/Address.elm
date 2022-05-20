module Plugin.View.Graph.Address exposing (..)

import Config.View as View
import Dict exposing (Dict)
import Json.Decode
import Model.Graph.Address exposing (Address)
import Msg.Graph exposing (Msg(..))
import Plugin as Plugin exposing (..)
import Plugin.Model as Plugin exposing (..)
import Svg.Styled as Svg exposing (Svg)


flags : Plugins -> View.Config -> Address -> List (Svg Msg)
flags plugins vc address =
    plugins
        |> Dict.toList
        |> List.filterMap
            (\( pid, plugin ) ->
                Dict.get pid address.plugins
                    |> plugin.view.graph.address.flags vc address.id
                    |> Maybe.map (Svg.map (PluginMsg pid (Plugin.Address address.id)))
            )



{-
   * allow plugins to store state related to things in graph
-}
