module Scenario.UnservedNetworksGsTest exposing (suite)

{-| Opening a saved graph with nodes on a network the backend does not serve
(lite networks switched off, or a currency not granted to the account).

Every request for such a network comes back 403, and the bulk requests of a
file load used to turn each one into a generic "Unexpected status code" toast.
The load now skips those requests, shows one notice naming the networks, keeps
their addresses on the graph (drawn faded) and writes their txs back out on
save, so re-saving the graph does not lose them.

-}

import Api.Data
import Dict
import Effect.Api
import Effect.Pathfinder
import Encode.Pathfinder
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Json.Decode
import Json.Encode
import Model exposing (Effect(..), Msg(..))
import Model.Notification as Notification
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)


{-| One btc and one arb address, and one arb tx, as `Encode.Pathfinder` writes them.
-}
btcAndArbGs : ( String, Json.Encode.Value )
btcAndArbGs =
    let
        id network value =
            Json.Encode.list Json.Encode.string [ network, value ]

        address network value x =
            Json.Encode.list identity
                [ id network value
                , Json.Encode.float x
                , Json.Encode.float 0
                , Json.Encode.bool False
                ]
    in
    ( "mixed.gs"
    , Json.Encode.list identity
        [ Json.Encode.string "pathfinder"
        , Json.Encode.string "1"
        , Json.Encode.string "mixed"
        , Json.Encode.list identity
            [ address "btc" "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" 0
            , address "arb" "0x9ed0c8bb9573f5eda630232521c5a2bc0b97369a" 200
            ]
        , Json.Encode.list identity
            [ Json.Encode.list identity
                [ id "arb" "5d40203feb69c0a559cf911d4c59a949ea6e83a917a744535aa6275e8c31c602"
                , Json.Encode.float 100
                , Json.Encode.float 0
                , Json.Encode.bool False
                , Json.Encode.int 0
                ]
            ]
        , Json.Encode.list identity []
        , Json.Encode.list identity []
        ]
    )


{-| The statistics fixture lists only btc: arb is not served.
-}
btcOnlyStats : Api.Data.Stats
btcOnlyStats =
    case Json.Decode.decodeString Api.Data.statsDecoder Fixture.stats of
        Ok stats ->
            stats

        Err _ ->
            { currencies = [], version = "", requestTimestamp = "" }


ready : App
ready =
    App.initAt "/pathfinder"
        |> App.step (BrowserGotCapabilities { networks = [] })
        |> App.step (BrowserGotStatistics btcOnlyStats)


opened : App
opened =
    ready |> App.step (BrowserGotDeserializedGS btcAndArbGs)


requestsFor : String -> Effect -> Bool
requestsFor network effect =
    case effect of
        PathfinderEffect (Effect.Pathfinder.ApiEffect (Effect.Api.BulkGetAddressEffect { currency } _)) ->
            currency == network

        PathfinderEffect (Effect.Pathfinder.ApiEffect (Effect.Api.BulkGetTxEffect { currency } _)) ->
            currency == network

        _ ->
            False


noticeMessage : App -> Maybe String
noticeMessage app =
    case Notification.peek (App.model app).notifications of
        Just (Notification.Info data) ->
            Just (data.message ++ " " ++ String.join "," data.variables)

        _ ->
            Nothing


savedTxs : App -> Result Json.Decode.Error (List ( String, String ))
savedTxs app =
    Encode.Pathfinder.encode (App.model app).pathfinder
        |> Json.Decode.decodeValue
            (Json.Decode.index 4
                (Json.Decode.list
                    (Json.Decode.index 0
                        (Json.Decode.map2 Tuple.pair
                            (Json.Decode.index 0 Json.Decode.string)
                            (Json.Decode.index 1 Json.Decode.string)
                        )
                    )
                )
            )


expectStatsFixtureHasOnlyBtc : Expectation
expectStatsFixtureHasOnlyBtc =
    btcOnlyStats.currencies
        |> List.map .name
        |> Expect.equal [ "btc" ]


suite : Test
suite =
    describe "opening a graph with nodes on an unserved network"
        [ test "the statistics fixture serves btc only" <|
            \_ -> expectStatsFixtureHasOnlyBtc
        , test "the load waits for the statistics" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (BrowserGotCapabilities { networks = [] })
                    |> App.step (BrowserGotDeserializedGS btcAndArbGs)
                    |> App.expectEffect "PostponeDeserializeEffect"
                        (\e ->
                            case e of
                                PostponeDeserializeEffect _ ->
                                    True

                                _ ->
                                    False
                        )
        , test "the served network is requested" <|
            \_ ->
                opened |> App.expectEffect "a btc bulk request" (requestsFor "btc")
        , test "the unserved network is not requested" <|
            \_ ->
                opened |> App.expectNoEffect "an arb bulk request" (requestsFor "arb")
        , test "both addresses stay on the graph" <|
            \_ ->
                (App.model opened).pathfinder.network.addresses
                    |> Dict.size
                    |> Expect.equal 2
        , test "one notice names the unserved network" <|
            \_ ->
                noticeMessage opened
                    |> Expect.equal (Just "gs-file-networks-not-on-account ARB")
        , test "with lite networks switched off the notice says so" <|
            \_ ->
                ready
                    |> App.mapModel
                        (\m ->
                            let
                                c =
                                    m.config
                            in
                            { m | config = { c | liteNetworks = False } }
                        )
                    |> App.step (BrowserGotDeserializedGS btcAndArbGs)
                    |> noticeMessage
                    |> Expect.equal (Just "gs-file-networks-lite-off ARB")
        , test "saving keeps the unfetched tx" <|
            \_ ->
                savedTxs opened
                    |> Expect.equal (Ok [ ( "arb", "5d40203feb69c0a559cf911d4c59a949ea6e83a917a744535aa6275e8c31c602" ) ])
        , test "a graph without unserved nodes shows no notice" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (BrowserGotCapabilities { networks = [] })
                    |> App.step (BrowserGotStatistics btcOnlyStats)
                    |> App.step
                        (BrowserGotDeserializedGS
                            ( "btc.gs"
                            , Json.Encode.list identity
                                [ Json.Encode.string "pathfinder"
                                , Json.Encode.string "1"
                                , Json.Encode.string "btc"
                                , Json.Encode.list identity
                                    [ Json.Encode.list identity
                                        [ Json.Encode.list Json.Encode.string [ "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" ]
                                        , Json.Encode.float 0
                                        , Json.Encode.float 0
                                        , Json.Encode.bool False
                                        ]
                                    ]
                                , Json.Encode.list identity []
                                , Json.Encode.list identity []
                                , Json.Encode.list identity []
                                ]
                            )
                        )
                    |> noticeMessage
                    |> Expect.equal Nothing
        ]
