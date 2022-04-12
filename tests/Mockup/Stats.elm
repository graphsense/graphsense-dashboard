module Mockup.Stats exposing (..)

import Api.Data
import Json.Encode exposing (encode)


stats : Api.Data.Stats
stats =
    { currencies = []
    , version = "1.0.0"
    , requestTimestamp = "123"
    }


statsEncoded : String
statsEncoded =
    stats
        |> Api.Data.encodeStats
        |> encode 0
