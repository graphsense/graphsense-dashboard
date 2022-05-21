module Plugin.Model exposing (..)

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Model.Graph.Id as Id


type alias PluginStates =
    Dict String Value


type UpdateContext
    = UpdateAddress
        Id.AddressId
        { graph : Value
        , address : Value
        }


type Context
    = Address Id.AddressId
    | Model


type OutMsg
    = ShowBrowser
