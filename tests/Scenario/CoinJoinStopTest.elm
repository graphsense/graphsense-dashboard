module Scenario.CoinJoinStopTest exposing (suite)

{-| Expanding an address onto a CoinJoin lands on the CoinJoin and stops there.

Normally an expand places the tx _and_ the biggest address on its far side
(`getAddressForDirection`). Through a CoinJoin that address is just the
largest participant, not where the funds came from or went to, so the graph
would show a link that does not exist on chain. The expand now places the
CoinJoin only and says why.

The control case is the same tx without the CoinJoin flag: there the far-side
address is still requested, which is what makes the assertions about the
CoinJoin case mean something.

-}

import Api.Data
import Dict
import Effect.Api
import Effect.Pathfinder
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Json.Decode
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Error exposing (Error(..), InfoError(..))
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Route.Pathfinder as Route
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)


anchor : Id
anchor =
    ( "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" )


mixHash : String
mixHash =
    "0c846ccdc95e77c247c4d09f2a59089a6ff6e00800ca4020662290e44531d97f"


biggestInput : String
biggestInput =
    "bc1qbiggestparticipantxxxxxxxxxxxxxxxxxxxx"


values : Int -> Api.Data.Values
values v =
    { fiatValues = [], value = v }


io : String -> Int -> Int -> Api.Data.TxValue
io address index v =
    { address = [ address ], index = Just index, value = values v }


{-| A three-party mix paying the anchor one of its equal outputs.
-}
mixTx : Bool -> Api.Data.TxUtxo
mixTx detected =
    { coinbase = False
    , currency = "btc"
    , height = 956067
    , inputs =
        Just
            [ io biggestInput 0 97726297
            , io "bc1qsmallparticipantaxxxxxxxxxxxxxxxxxxxxx" 1 30000
            , io "bc1qsmallparticipantbxxxxxxxxxxxxxxxxxxxxx" 2 25000
            ]
    , noInputs = 3
    , noOutputs = 3
    , outputs =
        Just
            [ io (Tuple.second anchor) 0 20000
            , io "bc1qotheroutputaxxxxxxxxxxxxxxxxxxxxxxxxxx" 1 20000
            , io "bc1qotheroutputbxxxxxxxxxxxxxxxxxxxxxxxxxx" 2 20000
            ]
    , timestamp = 1782819666
    , totalInput = values 97781297
    , totalOutput = values 60000
    , txHash = mixHash
    , txType = "utxo"
    , heuristics =
        Just
            { changeHeuristics = Nothing
            , coinjoinHeuristics =
                if detected then
                    Just
                        { consensus = Just { detected = True, confidence = 60, sources = [ "wasabi_coinjoin" ] }
                        , joinmarket = Nothing
                        , wasabi = Nothing
                        , whirlpoolTx0 = Nothing
                        , whirlpoolCoinjoin = Nothing
                        }

                else
                    Nothing
            }
    }


{-| Opens the anchor, expands it backward, and answers the "most recent
incoming tx" lookup and the tx fetch with `tx`.
-}
expandBackwardOnto : Api.Data.TxUtxo -> Api.Data.Address -> App
expandBackwardOnto tx address =
    Route.addressRoute { network = "btc", address = Tuple.second anchor }
        |> App.initAt
        |> App.respond
            (\eff ->
                case eff of
                    Effect.Api.GetAddressEffect _ toMsg ->
                        Just (toMsg address)

                    _ ->
                        Nothing
            )
        |> App.step (UserClickedAddressExpandHandle anchor Incoming)
        |> App.respond
            (\eff ->
                case eff of
                    Effect.Api.GetAddressTxsEffect _ toMsg ->
                        Just
                            (toMsg
                                { addressTxs =
                                    [ Api.Data.AddressTxAddressTxUtxo
                                        { coinbase = False
                                        , currency = "btc"
                                        , height = tx.height
                                        , timestamp = tx.timestamp
                                        , txHash = tx.txHash
                                        , txType = "utxo"
                                        , value = values 20000
                                        }
                                    ]
                                , nextPage = Nothing
                                }
                            )

                    _ ->
                        Nothing
            )
        |> App.respond
            (\eff ->
                case eff of
                    Effect.Api.GetTxEffect { txHash } toMsg ->
                        if txHash == tx.txHash then
                            Just (toMsg (Api.Data.TxTxUtxo tx))

                        else
                            Nothing

                    _ ->
                        Nothing
            )


withAddressFixture : (Api.Data.Address -> Expectation) -> Expectation
withAddressFixture f =
    case Json.Decode.decodeString Api.Data.addressDecoder Fixture.address of
        Ok address ->
            f address

        Err error ->
            Expect.fail ("the address fixture did not decode: " ++ Json.Decode.errorToString error)


requestsAddress : String -> Effect.Pathfinder.Effect -> Bool
requestsAddress addr eff =
    case eff of
        Effect.Pathfinder.ApiEffect (Effect.Api.GetAddressEffect req _) ->
            req.address == addr

        Effect.Pathfinder.BatchEffect batched ->
            List.any (requestsAddress addr) batched

        _ ->
            False


isMixerNotice : Effect.Pathfinder.Effect -> Bool
isMixerNotice eff =
    case eff of
        Effect.Pathfinder.ErrorEffect (InfoError (TxTracingThroughMixer ( "btc", hash ))) ->
            hash == mixHash

        Effect.Pathfinder.BatchEffect batched ->
            List.any isMixerNotice batched

        _ ->
            False


txOnGraph : App -> Bool
txOnGraph app =
    (App.model app).network.txs
        |> Dict.member ( "btc", mixHash )


suite : Test
suite =
    describe "expanding onto a CoinJoin"
        [ test "places the CoinJoin itself" <|
            \_ ->
                withAddressFixture <|
                    \address ->
                        expandBackwardOnto (mixTx True) address
                            |> txOnGraph
                            |> Expect.equal True
        , test "does not place its biggest input as if it were the source" <|
            \_ ->
                withAddressFixture <|
                    \address ->
                        expandBackwardOnto (mixTx True) address
                            |> App.expectNoEffect "a request for the biggest input" (requestsAddress biggestInput)
        , test "says why it stopped" <|
            \_ ->
                withAddressFixture <|
                    \address ->
                        expandBackwardOnto (mixTx True) address
                            |> App.expectEffect "the mixer notice" isMixerNotice
        , describe "the same tx without the CoinJoin flag"
            [ test "still places the biggest input" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            expandBackwardOnto (mixTx False) address
                                |> App.expectEffect "a request for the biggest input" (requestsAddress biggestInput)
            , test "and says nothing about mixing" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            expandBackwardOnto (mixTx False) address
                                |> App.expectNoEffect "the mixer notice" isMixerNotice
            ]
        ]
