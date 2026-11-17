module Scenario.BridgeUnsupportedAssetsTest exposing (suite)

{-| A complete bridge row whose token is not in the token registry (user
decision 2026-11-17) is drawn as a Bridge TX edge from the row alone: between
the row's payer and payee, keyed by the row, with no leg tx fetched (the
backends serve none for such a token) and no tx added. A registered side keeps
its ticker and scaled amount; an unregistered side is named by its contract,
marked "Unsupported token", and has no amount. Swaps and half rows stay notes.

The conversions are the adapter's recorded answers (var/bridge-maps/v2/api),
answered from either end of the pair.

-}

import Animation
import Api.Data
import Config.Update
import Dict exposing (Dict)
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Html.Attributes
import Html.Styled
import Init.Pathfinder.Id as Id
import Json.Decode
import Model.Locale exposing (State(..))
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Model
import RecordSetter exposing (s_locale, s_mapping, s_supportedTokens)
import Route.Pathfinder as Route
import Set
import Support.App as App exposing (App)
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Update.Pathfinder
import View.Pathfinder



-- RECORDED ANSWERS


recorded : String -> List Api.Data.ExternalConversion
recorded =
    Json.Decode.decodeString (Json.Decode.list Api.Data.externalConversionDecoder)
        >> Result.withDefault []


whEthSendArb : List Api.Data.ExternalConversion
whEthSendArb =
    recorded """[{"conversion_type": "dex_swap", "from_address": "0x000000000004444c5dc75cb358380d2e3de08a90", "to_address": "0x000000000004444c5dc75cb358380d2e3de08a90", "from_asset": "0x3010ccb5419f1ef26d40a7cd3f0d707a0fa127dc", "to_asset": "native", "from_amount": "0x16ff3019595ff7c5b1da", "to_amount": "0xf61b31aad74ec7", "from_asset_transfer": "0x5d3d5ceae42c16f11362ddcbead82ae7dbb9b3bfacec465fe6d806a86a4890fd_T102", "to_asset_transfer": "0x5d3d5ceae42c16f11362ddcbead82ae7dbb9b3bfacec465fe6d806a86a4890fd_I747", "from_network": "eth", "to_network": "eth", "from_is_supported_asset": false, "to_is_supported_asset": true}, {"conversion_type": "bridge_tx", "from_address": "0xd7ca08ec1aee9cce8a8eda9365343ef197674e1a", "to_address": "0xd7ca08ec1aee9cce8a8eda9365343ef197674e1a", "from_asset": "0x3010ccb5419f1ef26d40a7cd3f0d707a0fa127dc", "to_asset": "0x28bc3f91e153cffaf7f8c895784a8ea3e1730bf3", "from_amount": "0x16ff3019595ec8d6f800", "to_amount": "0x16ff3019595ec8d6f800", "from_asset_transfer": "5d3d5ceae42c16f11362ddcbead82ae7dbb9b3bfacec465fe6d806a86a4890fd_T104", "to_asset_transfer": "5c6c5a93e1c066edceb645b88ee2fef3d7338639cf021519189ea1b3e37bd3be_T1", "from_network": "eth", "to_network": "arb", "from_is_supported_asset": false, "to_is_supported_asset": false, "protocol": "wormhole", "transfer_id": "2/0000000000000000000000003ee18b2214aff97000d974cf647e7c347e8fa585/704561"}]"""


whEthRedeemWethFromBnb : List Api.Data.ExternalConversion
whEthRedeemWethFromBnb =
    recorded """[{"conversion_type": "bridge_tx", "from_address": "0x63ecf4af83c8991c4b07d37ed114516d84c0b4f6", "to_address": "0x63ecf4af83c8991c4b07d37ed114516d84c0b4f6", "from_asset": "0x4db5a66e937a9f4473fa95b1caf1d1e1d62e29ea", "to_asset": "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2", "from_amount": "0x3761c915bb400", "to_amount": "0x3761c915bb400", "from_asset_transfer": "c062beefe3919ee1661489934a0ba44e7f6ce7c84866b20d2d01acbf0d06f3d8_T189", "to_asset_transfer": "0e051329224a619c9dabc29502707e25ebcd142484ae75b47d766e880b477070_T43", "from_network": "bnb", "to_network": "eth", "from_is_supported_asset": false, "to_is_supported_asset": true, "protocol": "wormhole", "transfer_id": "4/000000000000000000000000b6f6d86a8f9879a9c87f643768d9efc38c1da6e7/893801"}]"""


whEthSendUsdtBnb : List Api.Data.ExternalConversion
whEthSendUsdtBnb =
    recorded """[{"conversion_type": "bridge_tx", "from_address": "0x4cc2c6e14c96aed6e99087c4477e4708a5db11a4", "to_address": "0x4cc2c6e14c96aed6e99087c4477e4708a5db11a4", "from_asset": "0xdac17f958d2ee523a2206206994597c13d831ec7", "to_asset": "0x524bc91dc82d6b90ef29f76a3ecaabafffd490bc", "from_amount": "0x11e4d1e4", "to_amount": "0x11e4d1e4", "from_asset_transfer": "2e0de754fc2cce7ec4af6dca60d3eea7c1c88fe35e178bfb22c6821f68d05173_T16", "to_asset_transfer": "da98070bb6562d7934a562b2e96bb16fd84c796b85f8779596bef68223601b02_T176", "from_network": "eth", "to_network": "bnb", "from_is_supported_asset": true, "to_is_supported_asset": false, "protocol": "wormhole", "transfer_id": "2/0000000000000000000000003ee18b2214aff97000d974cf647e7c347e8fa585/704946"}]"""


acEthFillWbtcFromArb : List Api.Data.ExternalConversion
acEthFillWbtcFromArb =
    recorded """[{"conversion_type": "bridge_tx", "from_address": "0x4c88c94f3d95386fe0d886b859239493f418dea9", "to_address": "0x4c88c94f3d95386fe0d886b859239493f418dea9", "from_asset": "0x2f2a2543b76a4166549f7aab2e75bef0aefc5b0f", "to_asset": "0x2260fac5e5542a773aa44fbcfedf7c193bc2c599", "from_amount": "0x7ec4c4", "to_amount": "0x7ea81d", "from_asset_transfer": "f5324870f7bd6bb1b9002c3bce15139a24e0ea9c9af09dc19673e7e43b243634_T1", "to_asset_transfer": "84d231e349ca68905843703e9bc642b26e87a549af75b7221f9ef1b29d376089_T123", "from_network": "arb", "to_network": "eth", "from_is_supported_asset": true, "to_is_supported_asset": false, "protocol": "across", "transfer_id": "4815032"}]"""


whBaseRedeemResend : List Api.Data.ExternalConversion
whBaseRedeemResend =
    recorded """[{"conversion_type": "bridge_tx", "from_address": "0x7b8460883aabf656feeb8af11a921d0002ecc43d", "to_address": "0x346ec0fadbf97cc3a5e2fbdaa7e4d10503f1052e", "from_asset": "0xc3e356c4f0ee6e93b0637811b1a30f7c43fc8e61", "to_asset": "0x51436f6bd047797de7d11e9d32685f029aed1069", "from_amount": "0x3503441abe", "to_amount": "0x3503441abe", "from_asset_transfer": "65daa706aef8793a40ef105f2cb06007046cc751ca8fc968c377849835804d2e_T8", "to_asset_transfer": "63ec822c051c7ec0433b21e596c58206bc170cc56e1a620ba74bd12a6ae84d54_T1", "from_network": "bnb", "to_network": "base", "from_is_supported_asset": false, "to_is_supported_asset": false, "protocol": "wormhole", "transfer_id": "4/000000000000000000000000b6f6d86a8f9879a9c87f643768d9efc38c1da6e7/896510"}, {"conversion_type": "bridge_tx", "from_address": "0xb2efa3d65c2d12c3e8ce7dbcfc0dae62289fb4c2", "to_address": "0x346ec0fadbf97cc3a5e2fbdaa7e4d10503f1052e", "from_asset": "0x51436f6bd047797de7d11e9d32685f029aed1069", "to_asset": "0xc3e356c4f0ee6e93b0637811b1a30f7c43fc8e61", "from_amount": "0x3503441abe", "to_amount": "0x3503441abe", "from_asset_transfer": "63ec822c051c7ec0433b21e596c58206bc170cc56e1a620ba74bd12a6ae84d54_T5", "to_asset_transfer": "af566c117b2cc1c7617eeb31b3eb37ed1867fa83129bdb94b9d68df0e2c83b91_T44", "from_network": "base", "to_network": "bnb", "from_is_supported_asset": false, "to_is_supported_asset": false, "protocol": "wormhole", "transfer_id": "30/0000000000000000000000008d2de8d2f73f1f4cab472ac9a881c9b123c79627/285095"}]"""


acEthFillUsdtFromLinea : List Api.Data.ExternalConversion
acEthFillUsdtFromLinea =
    recorded """[{"conversion_type": "bridge_tx", "from_address": "unknown", "to_address": "0x83470d93e5f039538fb238c62c5ac71a2410d7ca", "from_asset": "unknown", "to_asset": "0xdac17f958d2ee523a2206206994597c13d831ec7", "from_amount": "0x8cac906a", "to_amount": "0x8c9a910a", "from_asset_transfer": "unknown", "to_asset_transfer": "3d1be50dac5ba390dabb8b18de7f8ca3d19bfb0ddda18b760b08c1f28ba9c7d8_T311", "from_network": "unknown", "to_network": "eth", "from_is_supported_asset": false, "to_is_supported_asset": true, "protocol": "across", "transfer_id": "2300098"}]"""


bridgeRow : List Api.Data.ExternalConversion -> Api.Data.ExternalConversion
bridgeRow rows =
    rows
        |> List.filter (\c -> c.conversionType == Api.Data.ExternalConversionConversionTypeBridgeTx)
        |> List.head
        |> Maybe.withDefault emptyRow


emptyRow : Api.Data.ExternalConversion
emptyRow =
    { conversionType = Api.Data.ExternalConversionConversionTypeBridgeTx
    , fromAddress = ""
    , fromAmount = ""
    , fromAsset = ""
    , fromAssetTransfer = ""
    , fromIsSupportedAsset = False
    , fromNetwork = ""
    , toAddress = ""
    , toAmount = ""
    , toAsset = ""
    , toAssetTransfer = ""
    , toIsSupportedAsset = False
    , toNetwork = ""
    , fromAssetSymbol = Nothing
    , toAssetSymbol = Nothing
    , fromAssetDecimals = Nothing
    , toAssetDecimals = Nothing
    , fromAmountFiatValues = Nothing
    , toAmountFiatValues = Nothing
    , protocol = Nothing
    , protocolApp = Nothing
    , transferId = Nothing
    }



-- THE OPENED TX


sender : String
sender =
    "0x" ++ String.repeat 40 "1"


receiver : String
receiver =
    "0x" ++ String.repeat 40 "2"


{-| The tx a leg names, as `/txs/{hash}` serves it: its root transfer, from
`from` to `to`, with some native value of its own (a zero-value root sends the
deep link looking for sub-transfers instead).
-}
rootTx : String -> String -> String -> String -> Api.Data.Tx
rootTx network hash from to =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = network
        , fee = Nothing
        , fromAddress = from
        , height = 1
        , identifier = hash ++ "_I412"
        , isExternal = Just True
        , network = network
        , timestamp = 1788254088
        , toAddress = to
        , tokenTxId = Nothing
        , txHash = hash
        , txType = "account"
        , value = { fiatValues = [], value = 42 }
        }


{-| The tx of the row's source leg, or of its destination leg.
-}
sourceTx : Api.Data.ExternalConversion -> Api.Data.Tx
sourceTx c =
    rootTx c.fromNetwork (ConversionEdge.legTxHash c.fromAssetTransfer) sender receiver


destinationTx : Api.Data.ExternalConversion -> Api.Data.Tx
destinationTx c =
    rootTx c.toNetwork (ConversionEdge.legTxHash c.toAssetTransfer) sender receiver


type alias Opened =
    { app : App

    -- the answers to the address requests the opened tx made itself
    , ownAddresses : List Msg
    }


{-| The tx opened by its deep link, answered, and its conversions answered
with `rows`, as BridgeNotesTest does. The tx's own address requests are kept for later.
-}
open : Api.Data.Tx -> List Api.Data.ExternalConversion -> Opened
open tx rows =
    let
        ( network, hash ) =
            case tx of
                Api.Data.TxTxAccount t ->
                    ( t.network, t.txHash )

                Api.Data.TxTxUtxo t ->
                    ( t.currency, t.txHash )

        opened =
            Route.txRoute { network = network, txHash = hash }
                |> App.initAt
                |> App.respond
                    (\eff ->
                        case eff of
                            Effect.Api.GetTxEffect _ toMsg ->
                                Just (toMsg tx)

                            _ ->
                                Nothing
                    )
    in
    { app =
        case Dict.values (App.model opened).network.txs of
            node :: _ ->
                App.step (BrowserGotConversions Set.empty node rows) opened

            [] ->
                opened
    , ownAddresses = App.apiEffects opened |> List.filterMap answerAddress
    }


{-| Every address request answered, the conversions' and the tx's own.
-}
settle : Opened -> App
settle opened =
    opened.app
        |> App.respond answerAddress
        |> App.steps opened.ownAddresses


answerAddress : Effect.Api.Effect Msg -> Maybe Msg
answerAddress eff =
    case eff of
        Effect.Api.GetAddressEffect params toMsg ->
            Json.Decode.decodeString Api.Data.addressDecoder Fixture.address
                |> Result.toMaybe
                |> Maybe.map (\a -> toMsg { a | currency = params.currency, address = params.address })

        _ ->
            Nothing


requestedAddresses : App -> List Id
requestedAddresses app =
    App.apiEffects app
        |> List.filterMap
            (\eff ->
                case eff of
                    Effect.Api.GetAddressEffect params _ ->
                        Just (Id.init params.currency params.address)

                    _ ->
                        Nothing
            )


{-| Any request for a tx: a leg or a whole tx.
-}
txRequests : App -> Int
txRequests app =
    App.apiEffects app
        |> List.filter
            (\eff ->
                case eff of
                    Effect.Api.GetConversionLegEffect _ _ ->
                        True

                    Effect.Api.GetTxEffect _ _ ->
                        True

                    _ ->
                        False
            )
        |> List.length


edgeOf : Api.Data.ExternalConversion -> App -> Maybe ConversionEdge
edgeOf c app =
    Dict.get (ConversionEdge.rowEdgeId c) (App.model app).network.conversions


coordsOf : Id -> App -> Maybe ( Float, Float )
coordsOf id app =
    Dict.get id (App.model app).network.addresses
        |> Maybe.map (\a -> ( a.x, Animation.getTo a.y ))



-- THE RENDERED PAGE


{-| The registry of the tokens the recorded rows mark supported.
-}
registry : Dict String Api.Data.TokenConfigs
registry =
    let
        token contract ticker decimals =
            { contractAddress = Just contract, decimals = decimals, pegCurrency = Nothing, ticker = ticker }
    in
    Dict.fromList
        [ ( "eth"
          , { tokenConfigs =
                [ token "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2" "WETH" 18
                , token "0xdac17f958d2ee523a2206206994597c13d831ec7" "USDT" 6
                ]
            }
          )
        , ( "arb", { tokenConfigs = [ token "0x2f2a2543b76a4166549f7aab2e75bef0aefc5b0f" "WBTC" 8 ] } )
        ]


english : Dict String String
english =
    Dict.fromList
        [ ( "unsupported-swap-notice", "Note: Unsupported swap transaction detected, target {0} ({1})" )
        , ( "unsupported-bridge-notice", "Note: Unsupported bridge transaction detected, target {0} ({1})" )
        , ( "unsupported-protocol-bridge-notice", "Note: Unsupported {2} bridge transaction detected, target {0} ({1})" )
        ]


{-| What the backend serves with the lite networks switched off.
-}
ethOnly : Config.Update.Config
ethOnly =
    let
        uc =
            Env.updateConfig
    in
    { uc | networks = [ "eth" ] }


html : App -> Query.Single Msg
html app =
    let
        vc =
            Env.viewConfig
                |> s_locale
                    (Env.viewConfig.locale
                        |> s_mapping (Settled english)
                        |> s_supportedTokens registry
                    )
    in
    View.Pathfinder.view Plugin.Model.emptyModelState vc (App.model app)
        |> Html.Styled.div []
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


byTestId : String -> Selector
byTestId id =
    Selector.attribute (Html.Attributes.attribute "data-testid" id)


{-| The settled graph with the row's edge clicked.
-}
panel : Api.Data.ExternalConversion -> App -> App
panel c app =
    case edgeOf c app of
        Just edge ->
            App.step (UserClickedConversionEdge edge.id edge) app

        Nothing ->
            app



-- EXPECTATIONS


{-| From the tx of either leg: the edge is the row's, between its payer and
payee, drawn once both are loaded, with no leg tx asked for or added.
-}
expectDrawn : Api.Data.ExternalConversion -> Opened -> Expectation
expectDrawn c opened =
    let
        settled =
            settle opened
    in
    Expect.all
        [ \_ -> txRequests opened.app |> Expect.equal 0 |> Expect.onFail "no leg tx may be fetched"
        , \_ ->
            case edgeOf c settled of
                Just edge ->
                    Expect.all
                        [ \_ -> Expect.equal ( ConversionEdge.payerId c, ConversionEdge.payeeId c ) ( edge.inputAddressId, edge.outputAddressId )
                        , \_ -> Expect.equal c edge.raw
                        , \_ -> Expect.equal ( True, True ) ( edge.inputAddress /= Nothing, edge.outputAddress /= Nothing )
                        ]
                        ()

                Nothing ->
                    Expect.fail "the row is not drawn"
        , \_ -> Dict.size (App.model settled).network.txs |> Expect.equal 1 |> Expect.onFail "only the opened tx is on the graph"
        , \_ -> html settled |> Query.findAll [ byTestId "gs-swap-node" ] |> Query.count (Expect.equal 1)
        , \_ -> html settled |> Query.findAll [ byTestId "gs-conversion-notice" ] |> Query.count (Expect.equal 0)
        ]
        ()


bothEnds : String -> List Api.Data.ExternalConversion -> Test
bothEnds name rows =
    let
        c =
            bridgeRow rows
    in
    describe name
        [ test "from the source tx" <| \_ -> open (sourceTx c) [ c ] |> expectDrawn c
        , test "from the destination tx" <| \_ -> open (destinationTx c) [ c ] |> expectDrawn c
        ]


{-| The rendered edge label and the clicked edge's panel contain all of
`present` and none of `absent`.
-}
expectTexts : List String -> List String -> App -> Expectation
expectTexts present absent app =
    Expect.all
        (List.map (\t -> \q -> Query.has [ Selector.text t ] q) present
            ++ List.map (\t -> \q -> Query.hasNot [ Selector.text t ] q) absent
        )
        (html app)


suite : Test
suite =
    describe "bridges whose tokens are not in the registry"
        [ test "the recorded answers decode" <|
            \_ ->
                [ whEthSendArb, whEthRedeemWethFromBnb, whEthSendUsdtBnb, acEthFillWbtcFromArb, whBaseRedeemResend, acEthFillUsdtFromLinea ]
                    |> List.map List.length
                    |> Expect.equal [ 2, 1, 1, 1, 2, 1 ]
        , describe "a complete row is drawn as an edge between payer and payee"
            [ bothEnds "both tokens unsupported (wh-eth-send-arb)" whEthSendArb
            , bothEnds "source token unsupported (wh-eth-redeem-weth-from-bnb)" whEthRedeemWethFromBnb
            , bothEnds "destination token unsupported (wh-eth-send-usdt-bnb)" whEthSendUsdtBnb
            , bothEnds "destination token unsupported (ac-eth-fill-wbtc-from-arb)" acEthFillWbtcFromArb
            ]
        , test "an unsupported swap beside it keeps its note (wh-eth-send-arb)" <|
            \_ ->
                let
                    c =
                        bridgeRow whEthSendArb

                    settled =
                        open (sourceTx c) whEthSendArb |> settle
                in
                Expect.all
                    [ \_ -> edgeOf c settled |> Expect.notEqual Nothing
                    , \_ -> (App.model settled).network.conversions |> Dict.size |> Expect.equal 1
                    , \q -> q |> Query.findAll [ byTestId "gs-conversion-notice" ] |> Query.count (Expect.equal 1)
                    , \q -> q |> Query.has [ Selector.text "Note: Unsupported swap transaction detected, target 0x000000000004444c5dc75cb358380d2e3de08a90 (ETH)" ]
                    ]
                    (html settled)
        , test "both tokens unsupported: contracts on the edge, no amounts (wh-eth-send-arb)" <|
            \_ ->
                let
                    c =
                        bridgeRow whEthSendArb
                in
                open (sourceTx c) [ c ]
                    |> settle
                    |> panel c
                    |> expectTexts
                        [ "Bridge TX", "ETH-0x3010ccb5…27dc / ARB-0x28bc3f91…0bf3", "Unsupported tokens", "Wormhole", "Wormholescan" ]
                        [ c.fromAmount, c.toAmount ]
        , test "the registered side keeps ticker and amount, the other is named by contract (wh-eth-redeem-weth-from-bnb)" <|
            \_ ->
                let
                    c =
                        bridgeRow whEthRedeemWethFromBnb
                in
                open (destinationTx c) [ c ]
                    |> settle
                    |> panel c
                    |> expectTexts
                        [ "BNB-0x4db5a66e…29ea / ETH-WETH", "Unsupported token", "0.00097 WETH", "BNB-0x4db5a66e…29ea", "ETH-WETH" ]
                        [ c.fromAmount ]
        , test "a registered source keeps its amount (wh-eth-send-usdt-bnb)" <|
            \_ ->
                let
                    c =
                        bridgeRow whEthSendUsdtBnb
                in
                open (sourceTx c) [ c ]
                    |> settle
                    |> panel c
                    |> expectTexts
                        [ "ETH-USDT / BNB-0x524bc91d…90bc", "Unsupported token", "300.21 USDT" ]
                        [ c.toAmount ]
        , test "a registered arb WBTC keeps its amount, eth WBTC outside the registry has none (ac-eth-fill-wbtc-from-arb)" <|
            \_ ->
                let
                    c =
                        bridgeRow acEthFillWbtcFromArb
                in
                open (destinationTx c) [ c ]
                    |> settle
                    |> panel c
                    |> expectTexts
                        [ "ARB-WBTC / ETH-0x2260fac5…c599", "0.083 WBTC", "Across" ]
                        [ c.toAmount ]
        , test "the panel names both tokens with their full contracts to copy" <|
            \_ ->
                let
                    c =
                        bridgeRow whEthRedeemWethFromBnb
                in
                open (destinationTx c) [ c ]
                    |> settle
                    |> panel c
                    |> html
                    |> Query.findAll [ byTestId "gs-bridge-token" ]
                    |> Query.count (Expect.equal 2)
        , test "two rows in one tx are two edges (wh-base-redeem-resend)" <|
            \_ ->
                let
                    settled =
                        open (rootTx "base" "63ec822c051c7ec0433b21e596c58206bc170cc56e1a620ba74bd12a6ae84d54" sender receiver) whBaseRedeemResend
                            |> settle

                    edges =
                        List.filterMap (\c -> edgeOf c settled) whBaseRedeemResend

                    ends =
                        [ ( "base", "0x346ec0fadbf97cc3a5e2fbdaa7e4d10503f1052e" )
                        , ( "bnb", "0x7b8460883aabf656feeb8af11a921d0002ecc43d" )
                        , ( "base", "0xb2efa3d65c2d12c3e8ce7dbcfc0dae62289fb4c2" )
                        , ( "bnb", "0x346ec0fadbf97cc3a5e2fbdaa7e4d10503f1052e" )
                        ]
                in
                Expect.all
                    [ \_ -> List.length edges |> Expect.equal 2
                    , \_ -> (App.model settled).network.conversions |> Dict.size |> Expect.equal 2
                    , \_ -> edges |> List.all (\e -> e.inputAddress /= Nothing && e.outputAddress /= Nothing) |> Expect.equal True
                    , \_ ->
                        -- every end has a place of its own
                        ends
                            |> List.filterMap (\( n, a ) -> coordsOf (Id.init n a) settled)
                            |> Set.fromList
                            |> Set.size
                            |> Expect.equal 4
                    , \_ -> html settled |> Query.findAll [ byTestId "gs-swap-node" ] |> Query.count (Expect.equal 2)
                    ]
                    ()
        , test "the tx's own receiver is not asked for again, the other end goes below it" <|
            \_ ->
                let
                    c =
                        bridgeRow whEthRedeemWethFromBnb

                    opened =
                        open (rootTx "eth" (ConversionEdge.legTxHash c.toAssetTransfer) sender c.toAddress) [ c ]

                    settled =
                        settle opened
                in
                Expect.all
                    [ \_ -> requestedAddresses opened.app |> Expect.equal [ ConversionEdge.payerId c ]
                    , \_ ->
                        case ( coordsOf (ConversionEdge.payeeId c) settled, coordsOf (ConversionEdge.payerId c) settled ) of
                            ( Just ( nx, ny ), Just ( fx, fy ) ) ->
                                Expect.equal ( nx, True ) ( fx, fy > ny )

                            _ ->
                                Expect.fail "both ends should be on the graph"
                    , \_ -> edgeOf c settled |> Maybe.andThen .outputAddress |> Expect.notEqual Nothing
                    ]
                    ()
        , test "a half row keeps its note and loads nothing (ac-eth-fill-usdt-from-linea)" <|
            \_ ->
                let
                    c =
                        bridgeRow acEthFillUsdtFromLinea

                    opened =
                        open (destinationTx c) acEthFillUsdtFromLinea
                in
                Expect.all
                    [ \_ -> requestedAddresses opened.app |> Expect.equal []
                    , \_ -> txRequests opened.app |> Expect.equal 0
                    , \_ -> (App.model opened.app).network.conversions |> Dict.isEmpty |> Expect.equal True
                    , \_ ->
                        html (settle opened)
                            |> Query.has [ Selector.text "Note: Unsupported Across bridge transaction detected, target 0x83470d93e5f039538fb238c62c5ac71a2410d7ca (ETH)" ]
                    ]
                    ()
        , test "a row on a network the backend does not serve keeps its note" <|
            \_ ->
                let
                    c =
                        bridgeRow whEthRedeemWethFromBnb

                    tx =
                        destinationTx c

                    before =
                        open tx [] |> .app |> App.model

                    ( after, effects, _ ) =
                        case Dict.values before.network.txs of
                            node :: _ ->
                                Update.Pathfinder.update ethOnly (BrowserGotConversions Set.empty node [ c ]) before

                            [] ->
                                ( before, [], [] )
                in
                Expect.all
                    [ \_ -> after.network.conversions |> Dict.isEmpty |> Expect.equal True
                    , \_ -> after.network.txs |> Dict.values |> List.concatMap .unsupportedConversions |> List.length |> Expect.equal 1
                    , \_ -> List.length effects |> Expect.equal 0
                    ]
                    ()
        ]
