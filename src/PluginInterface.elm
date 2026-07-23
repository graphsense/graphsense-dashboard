module PluginInterface exposing (..)

import PluginInterface.Effects
import PluginInterface.Update
import PluginInterface.View


type alias PID =
    String


type alias Plugin flags modelState addressState msg addressMsg entityMsg =
    { view : PluginInterface.View.View modelState addressState msg
    , update : PluginInterface.Update.Update flags modelState addressState msg addressMsg entityMsg
    , effects : PluginInterface.Effects.Effects msg
    }


empty : Plugin flags modelState addressState msg addressMsg entityMsg
empty =
    { view = PluginInterface.View.init
    , update = PluginInterface.Update.init
    , effects = PluginInterface.Effects.init
    }
