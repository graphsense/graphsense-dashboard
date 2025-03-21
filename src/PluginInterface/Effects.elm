module PluginInterface.Effects exposing (Effects, init)


type alias Effects msg =
    { -- this command is triggered when user types into the search bar
      search : Maybe (String -> Cmd msg)
    }


init : Effects msg
init =
    { search = Nothing
    }
