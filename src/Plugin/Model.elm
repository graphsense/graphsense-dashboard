module Plugin.Model exposing (..)

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Model.Address exposing (Address)
import Model.Graph.Id as Id


type alias Config =
    { toUrl : String -> String
    }


type alias PluginStates =
    Dict String Value


type Context
    = Address Id.AddressId
    | Model


type OutMsg msg
    = ShowBrowser
    | UpdateAddresses Address msg
    | PushGraphUrl String
