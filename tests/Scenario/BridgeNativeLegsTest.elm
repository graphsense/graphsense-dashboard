module Scenario.BridgeNativeLegsTest exposing (suite)

{-| A native bridge leg names the BARE tx hash (adapter DASHBOARD\_CHANGES
D-37): there is no Transfer to point at, so the leg is the whole tx. `/txs/{hash}`
serves the whole tx as its root transfer `<hash>_I0`, so such a leg must resolve
to that node, and the Bridge TX edge must be drawn as for a `_T<n>` leg.

The conversions are the adapter's recorded answers (var/bridge-maps/v2/api).

-}

import Api.Data
import Dict
import Effect.Api
import Expect
import Json.Decode
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Set
import Support.App as App exposing (App)
import Support.SwapFixture exposing (txRequest)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector



-- RECORDED ANSWERS


{-| ac-eth-send-pol: an Across deposit of native ETH on eth, filled as WETH
(`_T111`) on pol.
-}
acrossEthSendPol : String
acrossEthSendPol =
    """
    {
      "conversion_type": "bridge_tx",
      "from_address": "0xfee1a21c1373577c08b49d543835e0346b138cae",
      "to_address": "0x89f423567c2648bb828c3997f60c47b54f57fa6e",
      "from_asset": "native",
      "to_asset": "0x7ceb23fd6bc0add59e62ac25578270cff1b9f619",
      "from_amount": "0x4536ee82b69ad8ac",
      "to_amount": "0x45359504e9c04729",
      "from_asset_transfer": "16f7467cb241ae6e9f82a71600e969e1677658603e45b576bfc964b7df6973af",
      "to_asset_transfer": "8c17c14e46f02badaf16816b5026425f4a9c11ea70acca7e9d5edebc9d79ae78_T111",
      "from_network": "eth",
      "to_network": "pol",
      "from_is_supported_asset": true,
      "to_is_supported_asset": true,
      "protocol": "across",
      "transfer_id": "1642578"
    }
    """


{-| ac-eth-fill-from-base: native ETH on base to native ETH on eth, both legs
bare hashes.
-}
acrossEthFillFromBase : String
acrossEthFillFromBase =
    """
    {
      "conversion_type": "bridge_tx",
      "from_address": "0xa9c9259ca0efe93e2af7bda7d8f438683d4b1fd6",
      "to_address": "0xa9c9259ca0efe93e2af7bda7d8f438683d4b1fd6",
      "from_asset": "native",
      "to_asset": "native",
      "from_amount": "0x3f67c07f4e7000",
      "to_amount": "0x3f02f5eaecdb25",
      "from_asset_transfer": "2c1cfd091e7f5f56854abf49d275cac5ec7eaba29aad86ea7e381eb1b1ef2542",
      "to_asset_transfer": "a89e63d97f3add6174a939845fdb8078da3f0e4a17c830b9f291176efcbddcb8",
      "from_network": "base",
      "to_network": "eth",
      "from_is_supported_asset": true,
      "to_is_supported_asset": true,
      "protocol": "across",
      "transfer_id": "5154354"
    }
    """


{-| A recorded row as the client decodes it; none if it does not decode, which
the first test reports.
-}
decode : String -> List Api.Data.ExternalConversion
decode =
    Json.Decode.decodeString Api.Data.externalConversionDecoder
        >> Result.toMaybe
        >> Maybe.map List.singleton
        >> Maybe.withDefault []



-- TXS


{-| A tx as `/txs/{identifier}` answers it. The txs endpoint serves hashes
without `0x`.
-}
tx : String -> String -> String -> String -> String -> Api.Data.Tx
tx network identifier from to currency =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = currency
        , fee = Nothing
        , fromAddress = from
        , height = 1
        , identifier = identifier
        , isExternal = Nothing
        , network = network
        , timestamp = 1788254088
        , toAddress = to
        , tokenTxId = Nothing
        , txHash = String.split "_" identifier |> List.head |> Maybe.withDefault identifier
        , txType = "account"
        , value = { fiatValues = [], value = 1000 }
        }


spokePool : String
spokePool =
    "0x5c7bcd6e7de5423a257d81b442095a1a6ced35c5"


{-| The whole eth deposit tx, as `/txs/16f7…` serves it: its root `_I0`.
-}
ethDepositRoot : Api.Data.Tx
ethDepositRoot =
    tx "eth" "16f7467cb241ae6e9f82a71600e969e1677658603e45b576bfc964b7df6973af_I0" "0xfee1a21c1373577c08b49d543835e0346b138cae" spokePool "eth"


polFill : Api.Data.Tx
polFill =
    tx "pol" "8c17c14e46f02badaf16816b5026425f4a9c11ea70acca7e9d5edebc9d79ae78_T111" spokePool "0x89f423567c2648bb828c3997f60c47b54f57fa6e" "weth"


baseDepositRoot : Api.Data.Tx
baseDepositRoot =
    tx "base" "2c1cfd091e7f5f56854abf49d275cac5ec7eaba29aad86ea7e381eb1b1ef2542_I0" "0xa9c9259ca0efe93e2af7bda7d8f438683d4b1fd6" spokePool "eth"


ethFillRoot : Api.Data.Tx
ethFillRoot =
    tx "eth" "a89e63d97f3add6174a939845fdb8078da3f0e4a17c830b9f291176efcbddcb8_I0" spokePool "0xa9c9259ca0efe93e2af7bda7d8f438683d4b1fd6" "eth"



-- DRIVING


identifierOf : Api.Data.Tx -> Id
identifierOf t =
    case t of
        Api.Data.TxTxAccount a ->
            ( a.network, a.identifier )

        Api.Data.TxTxUtxo u ->
            ( u.currency, u.txHash )


{-| The user opened the tx by its bare hash; the API answered the root.
-}
opened : Api.Data.Tx -> App -> App
opened t =
    let
        ( _, identifier ) =
            identifierOf t
    in
    App.step (BrowserGotTx (txRequest (String.split "_" identifier |> List.head |> Maybe.withDefault identifier)) t)


{-| `/txs/{tx}/conversions` of the tx on the graph answered `conversions`.
-}
gotConversions : Api.Data.Tx -> List Api.Data.ExternalConversion -> App -> App
gotConversions t conversions app =
    Dict.get (identifierOf t) (App.model app).network.txs
        |> Maybe.map (\node -> App.step (BrowserGotConversions Set.empty node conversions) app)
        |> Maybe.withDefault app


{-| The tx ids the last step asked the API for, as they go on the wire.
-}
requestedLegs : App -> List ( String, String )
requestedLegs =
    App.apiEffects
        >> List.filterMap
            (\eff ->
                case eff of
                    Effect.Api.GetConversionLegEffect { currency, txHash } _ ->
                        Just ( currency, txHash )

                    _ ->
                        Nothing
            )


answerLeg : Api.Data.Tx -> App -> App
answerLeg t =
    App.respond
        (\eff ->
            case eff of
                Effect.Api.GetConversionLegEffect _ toMsg ->
                    Just (toMsg t)

                _ ->
                    Nothing
        )


edges : App -> List ( Id, Id )
edges =
    App.model >> .network >> .conversions >> Dict.keys


{-| The bridge panel, opened by clicking the one edge.
-}
panel : App -> Query.Single Msg
panel app =
    case (App.model app).network.conversions |> Dict.toList of
        [ ( id, edge ) ] ->
            app
                |> App.step (UserClickedConversionEdge id edge)
                |> App.html

        _ ->
            App.html app


suite : Test
suite =
    describe "native bridge legs named by their bare tx hash"
        [ test "the recorded answers decode" <|
            \_ ->
                [ acrossEthSendPol, acrossEthFillFromBase ]
                    |> List.map (Json.Decode.decodeString Api.Data.externalConversionDecoder >> Result.map .protocol)
                    |> Expect.equal [ Ok (Just "across"), Ok (Just "across") ]
        , describe "a native source and a `_T<n>` destination (ac-eth-send-pol)"
            [ test "the deposit opened on eth asks pol for the fill" <|
                \_ ->
                    App.init
                        |> opened ethDepositRoot
                        |> gotConversions ethDepositRoot (decode acrossEthSendPol)
                        |> requestedLegs
                        |> Expect.equal [ ( "pol", "8c17c14e46f02badaf16816b5026425f4a9c11ea70acca7e9d5edebc9d79ae78_T111" ) ]
            , test "the edge runs from the whole deposit tx to the fill" <|
                \_ ->
                    App.init
                        |> opened ethDepositRoot
                        |> gotConversions ethDepositRoot (decode acrossEthSendPol)
                        |> answerLeg polFill
                        |> edges
                        |> Expect.equal [ ( identifierOf ethDepositRoot, identifierOf polFill ) ]
            , test "the fill opened on pol pairs with the whole deposit tx" <|
                \_ ->
                    App.init
                        |> opened polFill
                        |> gotConversions polFill (decode acrossEthSendPol)
                        |> answerLeg ethDepositRoot
                        |> edges
                        |> Expect.equal [ ( identifierOf ethDepositRoot, identifierOf polFill ) ]
            , test "the bridge panel names the bridge" <|
                \_ ->
                    App.init
                        |> opened ethDepositRoot
                        |> gotConversions ethDepositRoot (decode acrossEthSendPol)
                        |> answerLeg polFill
                        |> panel
                        |> Query.has [ Selector.text "Bridge transaction", Selector.text "Across" ]
            ]
        , describe "both legs native (ac-eth-fill-from-base)"
            [ test "the fill opened on eth asks base for the deposit, by its bare hash" <|
                \_ ->
                    App.init
                        |> opened ethFillRoot
                        |> gotConversions ethFillRoot (decode acrossEthFillFromBase)
                        |> requestedLegs
                        |> Expect.equal [ ( "base", "2c1cfd091e7f5f56854abf49d275cac5ec7eaba29aad86ea7e381eb1b1ef2542" ) ]
            , test "the edge runs from the base deposit to the eth fill" <|
                \_ ->
                    App.init
                        |> opened ethFillRoot
                        |> gotConversions ethFillRoot (decode acrossEthFillFromBase)
                        |> answerLeg baseDepositRoot
                        |> edges
                        |> Expect.equal [ ( identifierOf baseDepositRoot, identifierOf ethFillRoot ) ]
            , test "the deposit opened on base draws the same edge" <|
                \_ ->
                    App.init
                        |> opened baseDepositRoot
                        |> gotConversions baseDepositRoot (decode acrossEthFillFromBase)
                        |> answerLeg ethFillRoot
                        |> edges
                        |> Expect.equal [ ( identifierOf baseDepositRoot, identifierOf ethFillRoot ) ]
            , test "the bridge panel opens on that edge" <|
                \_ ->
                    App.init
                        |> opened ethFillRoot
                        |> gotConversions ethFillRoot (decode acrossEthFillFromBase)
                        |> answerLeg baseDepositRoot
                        |> panel
                        |> Query.has [ Selector.text "Bridge transaction", Selector.text "Across" ]
            ]
        ]
