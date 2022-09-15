module Plugin.Msg exposing (..)


import Plugin.Model
import PluginInterface.Msg as PluginInterface


type Msg
    = NoOp 

type AddressMsg
    = AddressMsg


type EntityMsg
    = EntityMsg


type alias OutMsg =
    PluginInterface.OutMsg Msg AddressMsg EntityMsg
