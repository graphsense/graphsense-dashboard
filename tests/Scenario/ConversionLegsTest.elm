module Scenario.ConversionLegsTest exposing (suite)

{-| Pins which sub-transactions a swap edge is drawn between.

The API answers `/txs/{id}/conversions` for a sub-tx id with the conversions that
name it as a leg, but for the whole tx (a bare hash, or the root trace `_I0`) it
returns EVERY conversion of the tx — whose legs are other sub-txs. The handler
used to assume the answered tx is always a leg, so the root got paired with the
output leg as a second, bogus swap edge whenever the root and a leg were both on
the graph (seen live: one bnb swap rendered as two "Swap BUSD / USD1" edges).

-}

import Api.Data
import Dict
import Effect.Api
import Expect
import Init.Pathfinder.Id as Id
import Model.Pathfinder.Network exposing (FindPosition(..))
import Msg.Pathfinder exposing (Msg(..), OutMsg(..))
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Util exposing (removeLeading0x)



-- FIXTURE: one dex swap inside one bnb tx


hash : String
hash =
    "63336a5ace33dc969cdb769f64b8499eae7f142741895fa4d589dbfa41bf5d95"


sender : String
sender =
    "0x53227a6d5a129143b6ad760810ece62c79ab8e97"


settlement : String
settlement =
    "0x9008d19f58aabd9ed0d60971565aa8510560ab41"


swapper : String
swapper =
    "0xe17ad4f88be9cd479a8036a058b893a0da62bc7a"


rootId : String
rootId =
    hash ++ "_I0"


inputLegId : String
inputLegId =
    hash ++ "_T95"


outputLegId : String
outputLegId =
    hash ++ "_T108"


accountTx : String -> String -> String -> Api.Data.Tx
accountTx identifier from to =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = "bnb"
        , fee = Nothing
        , fromAddress = from
        , height = 119317568
        , identifier = identifier
        , isExternal = Nothing
        , network = "bnb"
        , timestamp = 1788254088
        , toAddress = to
        , tokenTxId = Nothing
        , txHash = hash
        , txType = "account"
        , value = { fiatValues = [], value = 1 }
        }


rootTx : Api.Data.Tx
rootTx =
    accountTx rootId sender settlement


inputLegTx : Api.Data.Tx
inputLegTx =
    accountTx inputLegId swapper settlement


outputLegTx : Api.Data.Tx
outputLegTx =
    accountTx outputLegId settlement swapper


{-| The swap's leg identifiers carry a `0x` prefix, as the API serves dex swaps.
-}
swap : Api.Data.ExternalConversion
swap =
    { conversionType = Api.Data.ExternalConversionConversionTypeDexSwap
    , fromAddress = swapper
    , fromAmount = "0x16a4ecb955b8a31b"
    , fromAsset = "0xe9e7cea3dedca5984780bafc599bd69add087d56"
    , fromAssetTransfer = "0x" ++ inputLegId
    , fromIsSupportedAsset = True
    , fromNetwork = "bnb"
    , toAddress = swapper
    , toAmount = "0x16a56e085c4dad4f"
    , toAsset = "0x8d0d000ee44948fc98c9b98a4fa4921476f08b0d"
    , toAssetTransfer = "0x" ++ outputLegId
    , toIsSupportedAsset = True
    , toNetwork = "bnb"
    , fromAssetSymbol = Nothing
    , toAssetSymbol = Nothing
    , fromAssetDecimals = Nothing
    , toAssetDecimals = Nothing
    , fromAmountFiatValues = Nothing
    , toAmountFiatValues = Nothing
    }


{-| The same swap as the adapter serves it enriched: the native input leg
carries no display metadata, the token output leg carries its curated symbol
and decimals.
-}
curatedSwap : Api.Data.ExternalConversion
curatedSwap =
    { swap
        | fromAsset = "native"
        , toAssetSymbol = Just "USD1"
        , toAssetDecimals = Just 18
    }



-- DRIVING


identifierOf : Api.Data.Tx -> String
identifierOf tx =
    case tx of
        Api.Data.TxTxAccount t ->
            t.identifier

        Api.Data.TxTxUtxo t ->
            t.txHash


{-| Puts a tx on the graph the way an answered tx request does.
-}
withTx : Api.Data.Tx -> App -> App
withTx tx =
    App.step
        (BrowserGotTx
            { pos = Auto
            , loadAddresses = False
            , autoLinkInTraceMode = False
            , requestedTxHash = identifierOf tx
            }
            tx
        )


{-| The API answered `/txs/{identifier}/conversions` with these conversions.
-}
gotConversionsFor : String -> List Api.Data.ExternalConversion -> App -> App
gotConversionsFor identifier conversions app =
    Dict.get (Id.init "bnb" identifier) (App.model app).network.txs
        |> Maybe.map (\tx -> App.step (BrowserGotConversions tx conversions) app)
        |> Maybe.withDefault app


{-| The API answered `/txs/{identifier}/conversions` with the swap.
-}
gotSwapFor : String -> App -> App
gotSwapFor identifier =
    gotConversionsFor identifier [ swap ]


answerTxRequest : Api.Data.Tx -> App -> App
answerTxRequest tx =
    App.respond
        (\eff ->
            case eff of
                Effect.Api.GetTxEffect _ toMsg ->
                    Just (toMsg tx)

                _ ->
                    Nothing
        )


{-| The sub-tx ids the last step asked the API for, without the `0x` prefix.
-}
requestedTxs : App -> List String
requestedTxs app =
    App.apiEffects app
        |> List.filterMap
            (\eff ->
                case eff of
                    Effect.Api.GetTxEffect { txHash } _ ->
                        Just (removeLeading0x txHash)

                    _ ->
                        Nothing
            )


{-| The ids the last step asked `/conversions` for, without the `0x` prefix.
-}
requestedConversions : App -> List String
requestedConversions app =
    App.apiEffects app
        |> List.filterMap
            (\eff ->
                case eff of
                    Effect.Api.GetConversionEffect { txHash } _ ->
                        Just (removeLeading0x txHash)

                    _ ->
                        Nothing
            )


swapEdges : App -> List ( String, String )
swapEdges app =
    (App.model app).network.conversions
        |> Dict.keys
        |> List.map (\( ( _, input ), ( _, output ) ) -> ( input, output ))



-- SCENARIOS


suite : Test
suite =
    describe "swap edges are drawn between the swap's own legs"
        [ describe "which tx the swaps are asked for"
            [ test "a tx opened by its bare hash asks for every swap in the tx" <|
                \_ ->
                    App.init
                        |> App.step
                            (BrowserGotTx
                                { pos = Auto
                                , loadAddresses = False
                                , autoLinkInTraceMode = False
                                , requestedTxHash = hash
                                }
                                inputLegTx
                            )
                        |> requestedConversions
                        |> Expect.equal [ hash ]
            , test "a sub-transfer toggled on asks for its own swaps only" <|
                \_ ->
                    App.init
                        |> withTx inputLegTx
                        |> requestedConversions
                        |> Expect.equal [ inputLegId ]
            ]
        , describe "when a leg's conversions arrive"
            [ test "the input leg asks for the output leg" <|
                \_ ->
                    App.init
                        |> withTx inputLegTx
                        |> gotSwapFor inputLegId
                        |> requestedTxs
                        |> Expect.equal [ outputLegId ]
            , test "the output leg asks for the input leg" <|
                \_ ->
                    App.init
                        |> withTx outputLegTx
                        |> gotSwapFor outputLegId
                        |> requestedTxs
                        |> Expect.equal [ inputLegId ]
            , test "the edge runs from the input leg to the output leg" <|
                \_ ->
                    App.init
                        |> withTx inputLegTx
                        |> gotSwapFor inputLegId
                        |> answerTxRequest outputLegTx
                        |> swapEdges
                        |> Expect.equal [ ( inputLegId, outputLegId ) ]
            ]
        , describe "when the whole tx's conversions arrive on its root trace"
            [ test "the root asks for the input leg, since it is no leg itself" <|
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> gotSwapFor rootId
                        |> requestedTxs
                        |> Expect.equal [ inputLegId ]
            , test "the input leg then asks for the output leg" <|
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> gotSwapFor rootId
                        |> answerTxRequest inputLegTx
                        |> requestedTxs
                        |> Expect.equal [ outputLegId ]
            , test "the only edge runs between the two legs, never through the root" <|
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> gotSwapFor rootId
                        |> answerTxRequest inputLegTx
                        |> answerTxRequest outputLegTx
                        |> swapEdges
                        |> Expect.equal [ ( inputLegId, outputLegId ) ]
            , test "a leg served under an identifier the swap does not name stops the walk" <|
                -- the input leg is asked for by the identifier the swap names;
                -- were the backend to answer under a normalised one, pairing it
                -- would fail and the handler would ask for the very same leg
                -- again, forever
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> gotSwapFor rootId
                        |> answerTxRequest (accountTx (hash ++ "_T42") swapper settlement)
                        |> (\app -> ( requestedTxs app, Dict.size (App.model app).network.txs ))
                        |> Expect.equal ( [], 1 )
            , test "a leg already on the graph is reused rather than added twice" <|
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> withTx inputLegTx
                        |> gotSwapFor rootId
                        |> answerTxRequest inputLegTx
                        |> answerTxRequest outputLegTx
                        |> (\app -> ( swapEdges app, Dict.size (App.model app).network.txs ))
                        |> Expect.equal ( [ ( inputLegId, outputLegId ) ], 3 )
            ]
        , describe "a swap leg's curated symbol and decimals are registered"
            [ test "the token leg registers what the conversion carries" <|
                \_ ->
                    App.init
                        |> withTx inputLegTx
                        |> gotConversionsFor inputLegId [ curatedSwap ]
                        |> App.outMsgs
                        |> Expect.equal
                            [ RegisterConversionAsset "bnb"
                                { contractAddress = Just "0x8d0d000ee44948fc98c9b98a4fa4921476f08b0d"
                                , decimals = 18
                                , pegCurrency = Just "market"
                                , ticker = "USD1"
                                }
                            ]
            , test "a leg without symbol and decimals registers nothing" <|
                \_ ->
                    App.init
                        |> withTx inputLegTx
                        |> gotSwapFor inputLegId
                        |> App.outMsgs
                        |> Expect.equal []
            ]
        ]
