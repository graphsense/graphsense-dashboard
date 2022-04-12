module Mockup.Stats exposing (..)

import Api.Data
import Json.Encode exposing (encode)


stats : Api.Data.Stats
stats =
    { currencies =
        [ { name = "btc"
          , noAddressRelations = 200
          , noAddresses = 100
          , noBlocks = 150
          , noEntities = 100
          , noLabels = 50
          , noTaggedAddresses = 70
          , noTxs = 300
          , timestamp = 123
          }
        , { name = "ltc"
          , noAddressRelations = 2000
          , noAddresses = 1000
          , noBlocks = 1500
          , noEntities = 1000
          , noLabels = 500
          , noTaggedAddresses = 700
          , noTxs = 3000
          , timestamp = 1230
          }
        ]
    , version = "1.0.0"
    , requestTimestamp = "123"
    }


statsEncoded : String
statsEncoded =
    stats
        |> Api.Data.encodeStats
        |> encode 0
