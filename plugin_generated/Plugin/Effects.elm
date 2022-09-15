module Plugin.Effects exposing (..)

import Plugin.Msg
import PluginInterface.Effects


type alias Plugins =
    { 
    }


search : Plugins -> String -> List (Cmd Plugin.Msg.Msg)
search plugins query =
    [ 
    ]
        |> List.filterMap identity
