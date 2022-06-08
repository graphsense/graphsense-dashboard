module Plugin.Model exposing (..)

import Api.Data
import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Model.Address exposing (Address)
import Model.Entity exposing (Entity)
import Model.Graph.Id as Id


type alias Config =
    { toUrl : String -> String
    }


type alias PluginStates =
    Dict String Value


type Context
    = Address Id.AddressId
    | Model


type OutMsg msg addressMsg entityMsg
    = ShowBrowser
    | UpdateAddresses Address addressMsg
    | UpdateAddressEntities Address entityMsg
    | UpdateEntities Entity entityMsg
    | PushGraphUrl String
    | GetEntitiesForAddresses (List Address) (List ( Address, Api.Data.Entity ) -> msg)
    | GetEntities (List Entity) (List Api.Data.Entity -> msg)
