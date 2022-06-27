module Plugin.Effect exposing (..)

import Dict
import Model exposing (Msg(..))
import Plugin exposing (Plugins)


search : Plugins -> String -> List (Cmd Msg)
search plugins query =
    plugins
        |> Dict.toList
        |> List.map
            (\( pid, plugin ) ->
                plugin.effect.search query
                    |> Cmd.map (PluginMsg pid)
            )
