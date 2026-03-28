module Test.Update.Pathfinder.WorkflowNextUtxoTx exposing (suite)

import Api.Data
import Data.Api as Api
import Data.Pathfinder.Id as Id
import Effect.Api as ApiEffect
import Expect
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id as PathfinderId
import Test exposing (Test)
import Update.Pathfinder.WorkflowNextUtxoTx as WorkflowNextUtxoTx
import Workflow


suite : Test
suite =
    Test.describe "Update.Pathfinder.WorkflowNextUtxoTx"
        [ Test.test "outgoing continuation uses highest-value output for expanded address" <|
            \_ ->
                let
                    workflow =
                        WorkflowNextUtxoTx.start
                            { addressId = Id.address1
                            , direction = Outgoing
                            }
                            txWithExternalConsensusChange
                in
                case workflow of
                    Workflow.Next [ ApiEffect.ListSpentInTxRefsEffect req _ ] ->
                        Expect.equal (Just 0) req.index

                    _ ->
                        Expect.fail "Expected ListSpentInTxRefsEffect with the expanded-address output index"
        , Test.test "outgoing continuation falls back to own-address output index" <|
            \_ ->
                let
                    workflow =
                        WorkflowNextUtxoTx.start
                            { addressId = Id.address1
                            , direction = Outgoing
                            }
                            txWithOwnOutputOnly
                in
                case workflow of
                    Workflow.Next [ ApiEffect.ListSpentInTxRefsEffect req _ ] ->
                        Expect.equal (Just 0) req.index

                    _ ->
                        Expect.fail "Expected ListSpentInTxRefsEffect with own-address output index"
        , Test.test "outgoing continuation ignores external outputs when own-address output exists" <|
            \_ ->
                let
                    workflow =
                        WorkflowNextUtxoTx.start
                            { addressId = Id.address1
                            , direction = Outgoing
                            }
                            txWithoutConsensusUsesHighestNonSender
                in
                case workflow of
                    Workflow.Next [ ApiEffect.ListSpentInTxRefsEffect req _ ] ->
                        Expect.equal (Just 0) req.index

                    _ ->
                        Expect.fail "Expected ListSpentInTxRefsEffect to keep following expanded address output"
        ]


txWithExternalConsensusChange : Api.Data.TxUtxo
txWithExternalConsensusChange =
    { coinbase = False
    , currency = PathfinderId.network Id.tx1
    , height = 1
    , inputs = Just [ txValue [ PathfinderId.id Id.address1 ] 0 ]
    , noInputs = 1
    , noOutputs = 2
    , outputs =
        Just
            [ txValue [ PathfinderId.id Id.address1 ] 0
            , txValue [ "a9999999" ] 1
            ]
    , timestamp = 0
    , totalInput = Api.values
    , totalOutput = Api.values
    , txHash = PathfinderId.id Id.tx1
    , txType = "utxo"
    , heuristics =
        Just
            { changeHeuristics =
                Just
                    { consensus =
                        [ { output = { address = "a9999999", index = 1 }
                          , confidence = 95
                          , sources = [ "one_time_change" ]
                          }
                        ]
                    , oneTimeChange = Nothing
                    , directChange = Nothing
                    , multiInputChange = Nothing
                    }
            , coinjoinHeuristics = Nothing
            }
    }


txWithOwnOutputOnly : Api.Data.TxUtxo
txWithOwnOutputOnly =
    { coinbase = False
    , currency = PathfinderId.network Id.tx1
    , height = 1
    , inputs = Just [ txValue [ PathfinderId.id Id.address1 ] 0 ]
    , noInputs = 1
    , noOutputs = 1
    , outputs = Just [ txValue [ PathfinderId.id Id.address1 ] 0 ]
    , timestamp = 0
    , totalInput = Api.values
    , totalOutput = Api.values
    , txHash = PathfinderId.id Id.tx2
    , txType = "utxo"
    , heuristics = Nothing
    }


txWithoutConsensusUsesHighestNonSender : Api.Data.TxUtxo
txWithoutConsensusUsesHighestNonSender =
    { coinbase = False
    , currency = PathfinderId.network Id.tx1
    , height = 1
    , inputs = Just [ txValue [ PathfinderId.id Id.address1 ] 0 ]
    , noInputs = 1
    , noOutputs = 3
    , outputs =
        Just
            [ txValue [ PathfinderId.id Id.address1 ] 0
            , { address = [ "a2222222" ], index = Just 1, value = valuesWithAmount 90 }
            , { address = [ "a3333333" ], index = Just 2, value = valuesWithAmount 20 }
            ]
    , timestamp = 0
    , totalInput = Api.values
    , totalOutput = Api.values
    , txHash = PathfinderId.id Id.tx3
    , txType = "utxo"
    , heuristics = Nothing
    }


txValue : List String -> Int -> Api.Data.TxValue
txValue addresses index =
    { address = addresses
    , index = Just index
    , value = Api.values
    }


valuesWithAmount : Int -> Api.Data.Values
valuesWithAmount amount =
    let
        baseValues =
            Api.values
    in
    { baseValues | value = amount }
