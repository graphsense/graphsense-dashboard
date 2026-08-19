module Stub.Interface.Update exposing (plugin)

import Stub.Init
import Stub.Model
import Stub.Msg
import Stub.Update
import PluginInterface.Update


plugin : PluginInterface.Update.Update Stub.Model.Flags Stub.Model.Model Stub.Model.AddressState Stub.Msg.Msg Stub.Msg.AddressMsg Stub.Msg.EntityMsg
plugin =
    PluginInterface.Update.init
