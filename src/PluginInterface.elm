module PluginInterface exposing (..)

import Model
import PluginInterface.Effects
import PluginInterface.Update
import PluginInterface.View
import Svg.Styled exposing (..)


type alias PID =
    String


type alias Plugin modelState addressState entityState msg addressMsg entityMsg =
    { view : PluginInterface.View.View modelState addressState entityState msg
    , update : PluginInterface.Update.Update modelState addressState entityState msg addressMsg entityMsg
    , effects : PluginInterface.Effects.Effects msg
    }


empty : Plugin modelState addressState entityState msg addressMsg entityMsg
empty =
    { view = PluginInterface.View.init
    , update = PluginInterface.Update.init
    , effects = PluginInterface.Effects.init
    }
