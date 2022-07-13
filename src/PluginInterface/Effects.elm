module PluginInterface.Effects exposing (..)


type alias Effects msg =
    { search : Maybe (String -> Cmd msg)
    }


init : Effects msg
init =
    { search = Nothing
    }
