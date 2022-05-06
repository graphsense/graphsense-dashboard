module Store.Effect exposing (..)

import Api.Data
import Store.Msg exposing (Msg)


type Effect
    = GetAddressEffect { currency : String, address : String, toMsg : Api.Data.Address -> Msg }
    | NoEffect


n m =
    ( m, NoEffect )
