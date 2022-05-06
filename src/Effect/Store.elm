module Effect.Store exposing (..)

import Api.Data
import Msg.Store exposing (Msg)


type Effect
    = GetAddressEffect { currency : String, address : String, toMsg : Api.Data.Address -> Msg }
    | NoEffect


n m =
    ( m, NoEffect )
