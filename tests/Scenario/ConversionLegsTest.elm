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
import Effect.Pathfinder
import Expect
import Http
import Init.Pathfinder.Id as Id
import Model
import Model.Dialog as Dialog
import Model.Notification as Notification
import Model.Pathfinder.Network exposing (FindPosition(..))
import Msg.Pathfinder exposing (Msg(..), OutMsg(..))
import Support.App as App exposing (App)
import Support.MainApp as MainApp
import Test exposing (Test, describe, test)
import Update.Statusbar as Statusbar
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


{-| A native input leg the adapter synthesized (DASHBOARD\_CHANGES D-27): its
`_I<k>` index sits at or above the adapter's SYNTHESIZED\_INDEX\_BASE (1048576).
The id is opaque: the dashboard must ask for it exactly as served, never parse
or display `k`, and treat a 404 (a leg that cannot be reconstructed) as the end
of the walk.
-}
synthesizedLegId : String
synthesizedLegId =
    hash ++ "_I1048583"


synthesizedLegTx : Api.Data.Tx
synthesizedLegTx =
    accountTx synthesizedLegId swapper settlement


nativeInSwap : Api.Data.ExternalConversion
nativeInSwap =
    { swap
        | fromAsset = "native"
        , fromAssetTransfer = "0x" ++ synthesizedLegId
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

                Effect.Api.GetConversionLegEffect _ toMsg ->
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

                    Effect.Api.GetConversionLegEffect { txHash } _ ->
                        Just (removeLeading0x txHash)

                    _ ->
                        Nothing
            )


{-| The tx ids the last step asked the API for, exactly as they go on the wire.
-}
requestedTxsVerbatim : App -> List String
requestedTxsVerbatim app =
    App.apiEffects app
        |> List.filterMap
            (\eff ->
                case eff of
                    Effect.Api.GetTxEffect { txHash } _ ->
                        Just txHash

                    Effect.Api.GetConversionLegEffect { txHash } _ ->
                        Just txHash

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



-- DRIVING THE WHOLE DASHBOARD


{-| The shell's side of a failed synthesized-leg fetch. The output leg is on the
graph and its conversions name the synthesized input leg, so the Pathfinder
asks for that leg. Statusbar tokens are handed out as `Main.main` does before
performing, and the one API request that step made is answered with a 404.
`before` is the state the request was sent from, `after` the state once the
404 is handled.
-}
shellLegNotFound : { before : MainApp.App, after : MainApp.App }
shellLegNotFound =
    let
        withOutputLeg =
            MainApp.initAt "/"
                |> MainApp.step
                    (Model.PathfinderMsg
                        (BrowserGotTx
                            { pos = Auto
                            , loadAddresses = False
                            , autoLinkInTraceMode = False
                            , requestedTxHash = outputLegId
                            }
                            outputLegTx
                        )
                    )

        asked =
            Dict.get (Id.init "bnb" outputLegId) (MainApp.model withOutputLeg).pathfinder.network.txs
                |> Maybe.map (\tx -> MainApp.step (Model.PathfinderMsg (BrowserGotConversions tx [ nativeInSwap ])) withOutputLeg)
                |> Maybe.withDefault withOutputLeg

        ( tokened, tagged ) =
            Statusbar.messagesFromEffects (MainApp.model asked) (MainApp.effects asked)

        before =
            MainApp.mapModel (always tokened) asked

        after =
            case List.filterMap apiRequest tagged of
                [ ( token, request ) ] ->
                    answer404 token request before

                _ ->
                    -- not exactly the one leg request: nothing is answered,
                    -- and the statusbar test fails on the entry left open
                    before
    in
    { before = before, after = after }


apiRequest : ( Maybe String, Model.Effect ) -> Maybe ( Maybe String, Effect.Api.Effect Model.Msg )
apiRequest ( token, eff ) =
    case eff of
        Model.ApiEffect request ->
            Just ( token, request )

        Model.PathfinderEffect (Effect.Pathfinder.ApiEffect request) ->
            Just ( token, Effect.Api.map Model.PathfinderMsg request )

        _ ->
            Nothing


answer404 : Maybe String -> Effect.Api.Effect Model.Msg -> MainApp.App -> MainApp.App
answer404 token request =
    MainApp.step
        (Model.BrowserGotResponseWithHeaders token
            (Err ( Http.BadStatus 404, Dict.empty, request ))
        )


dialogShown : MainApp.App -> Maybe Dialog.ErrorType
dialogShown app =
    case (MainApp.model app).dialog of
        Just (Dialog.Error { type_ }) ->
            Just type_

        Just _ ->
            Just (Dialog.General { title = "some other dialog", message = "", variables = [] })

        Nothing ->
            Nothing


isNotificationEffect : Model.Effect -> Bool
isNotificationEffect eff =
    case eff of
        Model.NotificationEffect _ ->
            True

        _ ->
            False



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
            , test "the output leg answered with itself is never paired with itself" <|
                -- it asked for the input leg; a backend serving the output leg
                -- back must not produce an edge from a tx to itself
                \_ ->
                    App.init
                        |> withTx outputLegTx
                        |> gotSwapFor outputLegId
                        |> answerTxRequest outputLegTx
                        |> swapEdges
                        |> Expect.equal []
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
            , test "the input-leg request answered with the output leg stops the walk" <|
                -- the root asked for the INPUT leg; taking the output leg in its
                -- place would ask for the input leg again, and a second output-leg
                -- answer would then be paired with itself
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> gotSwapFor rootId
                        |> answerTxRequest outputLegTx
                        |> (\app -> ( requestedTxs app, swapEdges app, Dict.size (App.model app).network.txs ))
                        |> Expect.equal ( [], [], 1 )
            , test "an output leg answered twice never becomes a self swap edge" <|
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> gotSwapFor rootId
                        |> answerTxRequest outputLegTx
                        |> answerTxRequest outputLegTx
                        |> swapEdges
                        |> List.filter (\( input, output ) -> input == output)
                        |> Expect.equal []
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
        , describe "a synthesized native leg (D-27) is a normal, opaque leg"
            [ test "a synthesized native leg id is fetched verbatim" <|
                \_ ->
                    App.init
                        |> withTx outputLegTx
                        |> gotConversionsFor outputLegId [ nativeInSwap ]
                        |> requestedTxsVerbatim
                        |> Expect.equal [ nativeInSwap.fromAssetTransfer ]
            , test "the root trace asks for the synthesized leg verbatim too" <|
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> gotConversionsFor rootId [ nativeInSwap ]
                        |> requestedTxsVerbatim
                        |> Expect.equal [ nativeInSwap.fromAssetTransfer ]
            , test "an answered synthesized leg becomes a node paired with the output leg" <|
                \_ ->
                    App.init
                        |> withTx outputLegTx
                        |> gotConversionsFor outputLegId [ nativeInSwap ]
                        |> answerTxRequest synthesizedLegTx
                        |> (\app -> ( swapEdges app, Dict.size (App.model app).network.txs ))
                        |> Expect.equal ( [ ( synthesizedLegId, outputLegId ) ], 2 )
            ]
        , describe "a synthesized leg the API answers with 404 (D-27) ends the walk quietly"
            [ test "no dialog opens" <|
                \_ ->
                    shellLegNotFound
                        |> .after
                        |> dialogShown
                        |> Expect.equal Nothing
            , test "no notification is raised" <|
                \_ ->
                    shellLegNotFound
                        |> (\{ before, after } ->
                                ( Notification.peek (MainApp.model after).notifications
                                    == Notification.peek (MainApp.model before).notifications
                                , MainApp.effects after |> List.filter isNotificationEffect |> List.length
                                )
                           )
                        |> Expect.equal ( True, 0 )
            , test "the statusbar entry is cleared without an error" <|
                \_ ->
                    shellLegNotFound
                        |> .after
                        |> MainApp.model
                        |> .statusbar
                        |> (\sb ->
                                ( Dict.size sb.messages
                                , sb.log |> List.filter (\( _, _, err ) -> err /= Nothing) |> List.length
                                )
                           )
                        |> Expect.equal ( 0, 0 )
            , test "the graph keeps the output leg and gets no swap edge" <|
                \_ ->
                    shellLegNotFound
                        |> .after
                        |> MainApp.model
                        |> .pathfinder
                        |> .network
                        |> (\network ->
                                ( Dict.keys network.conversions
                                , Dict.keys network.txs |> List.map Tuple.second
                                )
                           )
                        |> Expect.equal ( [], [ outputLegId ] )
            , test "a 404 on a tx the user asked for still says the tx was not found" <|
                -- the carve-out is for the swap walk only
                \_ ->
                    let
                        lookup =
                            Effect.Api.GetTxEffect
                                { currency = "bnb"
                                , txHash = synthesizedLegId
                                , includeIo = True
                                , tokenTxId = Nothing
                                }
                                (\tx ->
                                    Model.PathfinderMsg
                                        (BrowserGotTx
                                            { pos = Auto
                                            , loadAddresses = False
                                            , autoLinkInTraceMode = False
                                            , requestedTxHash = synthesizedLegId
                                            }
                                            tx
                                        )
                                )

                        start =
                            MainApp.initAt "/"

                        ( tokened, tagged ) =
                            Statusbar.messagesFromEffects (MainApp.model start) [ Model.ApiEffect lookup ]
                    in
                    MainApp.mapModel (always tokened) start
                        |> answer404 (List.head tagged |> Maybe.andThen Tuple.first) lookup
                        |> dialogShown
                        |> Expect.equal (Just (Dialog.TxNotFound [ synthesizedLegId ]))
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
