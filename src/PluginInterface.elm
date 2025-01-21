module PluginInterface exposing (..)

import PluginInterface.Effects
import PluginInterface.Update
import PluginInterface.View


type alias PID =
    String


type alias Plugin flags modelState addressState entityState msg addressMsg entityMsg dialogType =
    { view : PluginInterface.View.View modelState addressState entityState dialogType msg
    , update : PluginInterface.Update.Update flags modelState addressState entityState msg addressMsg entityMsg
    , effects : PluginInterface.Effects.Effects msg
    }


empty : Plugin flags modelState addressState entityState msg addressMsg entityMsg dialogType
empty =
    { view = PluginInterface.View.init
    , update = PluginInterface.Update.init
    , effects = PluginInterface.Effects.init
    }
