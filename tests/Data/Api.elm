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
    let
        outputs =
            [ { address = [ Id.address3 |> Id.id ], value = values }
            , { address = [ Id.address4 |> Id.id ], value = values }
            , { address = [ Id.address5 |> Id.id ], value = values }
            ]

        inputs =
            [ { address = [ Id.address1 |> Id.id ], value = values }
            ]
    in
    Api.Data.TxTxUtxo
        { coinbase = False
        , currency = Id.network Id.tx1
        , height = 1
        , inputs = Just inputs
        , noInputs = List.length inputs
        , noOutputs = List.length outputs
        , outputs = Just outputs
        , timestamp = 0
        , totalInput = values
        , totalOutput = values
        , txHash = Id.id Id.tx1
        , txType = "utxo"
        }


tx2 : Api.Data.Tx
tx2 =
    let
        inputs =
            [ { address = [ Id.address6 |> Id.id ], value = values }
            ]

        outputs =
            [ { address = [ Id.address1 |> Id.id ], value = values }
            ]
    in
    Api.Data.TxTxUtxo
        { coinbase = False
        , currency = Id.network Id.tx2
        , height = 1
        , inputs = Just inputs
        , noInputs = List.length inputs
        , noOutputs = List.length outputs
        , outputs = Just outputs
        , timestamp = 0
        , totalInput = values
        , totalOutput = values
        , txHash = Id.id Id.tx2
        , txType = "utxo"
        }