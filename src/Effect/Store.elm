module Effect.Store exposing (Effect(..))

import Api.Data
import Msg.Store exposing (Msg)


type Effect
    = GetEntityForAddressEffect { currency : String, address : String, toMsg : Api.Data.Entity -> Msg }
