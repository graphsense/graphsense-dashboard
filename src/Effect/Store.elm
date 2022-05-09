module Effect.Store exposing (..)

import Api.Data
import Msg.Store exposing (Msg)


type Effect
    = GetAddressEffect { currency : String, address : String, toMsg : Api.Data.Address -> Msg }
    | GetEntityEffect { currency : String, entity : Int, toMsg : Api.Data.Entity -> Msg }
    | GetEntityForAddressEffect { currency : String, address : String, toMsg : Api.Data.Entity -> Msg }
