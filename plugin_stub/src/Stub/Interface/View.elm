module Stub.Interface.View exposing (plugin)

import Stub.Model
import Stub.Msg
import Stub.View
import PluginInterface.View


plugin : PluginInterface.View.View Stub.Model.Model Stub.Model.AddressState Stub.Msg.Msg
plugin =
    PluginInterface.View.init
