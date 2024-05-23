module Data.Api exposing (..)

import Api.Data
import Data.Pathfinder.Id as Id
import Model.Pathfinder.Id as Id


values : Api.Data.Values
values =
    { fiatValues = []
    , value = 0
    }


tx1 : Api.Data.Tx
tx1 =
    Api.Data.TxTxUtxo
        { coinbase = False
        , currency = Id.network Id.tx1
        , height = 1
        , inputs =
            Just
                [ { address = [ Id.address1 |> Id.id ], value = values }
                ]
        , noInputs = 1
        , noOutputs = 2
        , outputs =
            Just
                [ { address = [ Id.address3 |> Id.id ], value = values }
                , { address = [ Id.address4 |> Id.id ], value = values }
                , { address = [ Id.address5 |> Id.id ], value = values }
                ]
        , timestamp = 0
        , totalInput = values
        , totalOutput = values
        , txHash = Id.id Id.tx1
        , txType = "utxo"
        }
