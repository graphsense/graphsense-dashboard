module Stub.Interface.Effects exposing (plugin)

import Stub.Msg
import PluginInterface.Effects


plugin : PluginInterface.Effects.Effects Stub.Msg.Msg
plugin =
    PluginInterface.Effects.init
