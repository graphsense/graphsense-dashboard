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
        [ Test.test "outgoing continuation uses highest-value output for expanded address (BiggestByValue)" <|
            \_ ->
                let
                    workflow =
                        WorkflowNextUtxoTx.start
                            { addressId = Id.address1
                            , direction = Outgoing
                            , indexSelection = WorkflowNextUtxoTx.BiggestByValue
                            }
                            txWithExternalConsensusChange
                in
                case workflow of
                    Workflow.Next [ ApiEffect.ListSpentInTxRefsEffect req _ ] ->
                        Expect.equal (Just 0) req.index

                    _ ->
                        Expect.fail "Expected ListSpentInTxRefsEffect with the expanded-address output index"
        , Test.test "outgoing continuation falls back to own-address output index (BiggestByValue)" <|
            \_ ->
                let
                    workflow =
                        WorkflowNextUtxoTx.start
                            { addressId = Id.address1
                            , direction = Outgoing
                            , indexSelection = WorkflowNextUtxoTx.BiggestByValue
                            }
                            txWithOwnOutputOnly
                in
                case workflow of
                    Workflow.Next [ ApiEffect.ListSpentInTxRefsEffect req _ ] ->
                        Expect.equal (Just 0) req.index

                    _ ->
                        Expect.fail "Expected ListSpentInTxRefsEffect with own-address output index"
        , Test.test "outgoing continuation ignores external outputs when own-address output exists (BiggestByValue)" <|
            \_ ->
                let
                    workflow =
                        WorkflowNextUtxoTx.start
                            { addressId = Id.address1
                            , direction = Outgoing
                            , indexSelection = WorkflowNextUtxoTx.BiggestByValue
                            }
                            txWithoutConsensusUsesHighestNonSender
                in
                case workflow of
                    Workflow.Next [ ApiEffect.ListSpentInTxRefsEffect req _ ] ->
                        Expect.equal (Just 0) req.index

                    _ ->
                        Expect.fail "Expected ListSpentInTxRefsEffect to keep following expanded address output"
        , Test.test "outgoing continuation with Specific index picks that index" <|
            \_ ->
                let
                    workflow =
                        WorkflowNextUtxoTx.start
                            { addressId = Id.address1
                            , direction = Outgoing
                            , indexSelection = WorkflowNextUtxoTx.Specific 1
                            }
                            txWithMultipleOwnOutputs
                in
                case workflow of
                    Workflow.Next [ ApiEffect.ListSpentInTxRefsEffect req _ ] ->
                        Expect.equal (Just 1) req.index

                    _ ->
                        Expect.fail "Expected ListSpentInTxRefsEffect with specific index 1"
        , Test.test "outgoing continuation with AllMatching follows all outputs" <|
            \_ ->
                let
                    workflow =
                        WorkflowNextUtxoTx.start
                            { addressId = Id.address1
                            , direction = Outgoing
                            , indexSelection = WorkflowNextUtxoTx.AllMatching
                            }
                            txWithMultipleOwnOutputs
                in
                case workflow of
                    Workflow.Next effects ->
                        let
                            indices =
                                effects
                                    |> List.filterMap
                                        (\effect ->
                                            case effect of
                                                ApiEffect.ListSpentInTxRefsEffect req _ ->
                                                    req.index

                                                _ ->
                                                    Nothing
                                        )
                                    |> List.sort
                        in
                        Expect.equal indices [ 0, 1 ]

                    _ ->
                        Expect.fail "Expected Workflow.Next with multiple effects"
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


txWithMultipleOwnOutputs : Api.Data.TxUtxo
txWithMultipleOwnOutputs =
    { coinbase = False
    , currency = PathfinderId.network Id.tx1
    , height = 1
    , inputs = Just [ txValue [ PathfinderId.id Id.address1 ] 0 ]
    , noInputs = 1
    , noOutputs = 2
    , outputs =
        Just
            [ txValue [ PathfinderId.id Id.address1 ] 0
            , txValue [ PathfinderId.id Id.address1 ] 1
            ]
    , timestamp = 0
    , totalInput = Api.values
    , totalOutput = Api.values
    , txHash = PathfinderId.id Id.tx4
    , txType = "utxo"
    , heuristics = Nothing
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
