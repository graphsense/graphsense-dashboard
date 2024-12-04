module Data.Api exposing (tx1, tx2, tx3, tx4, values)

import Api.Data
import Data.Pathfinder.Id as Id
import Model.Pathfinder.Id as Id


values : Api.Data.Values
values =
    { fiatValues = []
    , value = 0
    }


tx1 : Api.Data.TxUtxo
tx1 =
    let
        outputs =
            [ { address = [ Id.address3 |> Id.id ], value = values }
            , { address = [ Id.address4 |> Id.id ], value = values }
            , { address = [ Id.address5 |> Id.id ], value = values }
            , { address = [ Id.address8 |> Id.id ], value = values }
            ]

        inputs =
            [ { address = [ Id.address1 |> Id.id ], value = values }
            ]
    in
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


tx2 : Api.Data.TxUtxo
tx2 =
    let
        inputs =
            [ { address = [ Id.address6 |> Id.id ], value = values }
            ]

        outputs =
            [ { address = [ Id.address1 |> Id.id ], value = values }
            ]
    in
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


tx3 : Api.Data.TxUtxo
tx3 =
    let
        inputs =
            [ { address = [ Id.address1 |> Id.id ], value = values }
            ]

        outputs =
            [ { address = [ Id.address7 |> Id.id ], value = values }
            ]
    in
    { coinbase = False
    , currency = Id.network Id.tx3
    , height = 1
    , inputs = Just inputs
    , noInputs = List.length inputs
    , noOutputs = List.length outputs
    , outputs = Just outputs
    , timestamp = 0
    , totalInput = values
    , totalOutput = values
    , txHash = Id.id Id.tx3
    , txType = "utxo"
    }


tx4 : Api.Data.TxUtxo
tx4 =
    let
        inputs =
            [ { address = [ Id.address3 |> Id.id ], value = values }
            ]

        outputs =
            [ { address = [ Id.address8 |> Id.id ], value = values }
            ]
    in
    { coinbase = False
    , currency = Id.network Id.tx4
    , height = 1
    , inputs = Just inputs
    , noInputs = List.length inputs
    , noOutputs = List.length outputs
    , outputs = Just outputs
    , timestamp = 0
    , totalInput = values
    , totalOutput = values
    , txHash = Id.id Id.tx4
    , txType = "utxo"
    }
