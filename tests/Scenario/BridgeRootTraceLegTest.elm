module Scenario.BridgeRootTraceLegTest exposing (suite)

{-| A native bridge leg names the BARE tx hash (adapter DASHBOARD\_CHANGES
D-37), meaning the whole tx. `/txs/{hash}` serves the whole tx as its ROOT
transfer, `<hash>_I<root trace>` flagged `is_external`, and the root trace is
`_I0` only when it happens to be the first trace of its block. On eth it mostly
is not: the live answers below are `_I412` and `_I573`.

Replays the live flow: the tx opened by its deep link, the API's answers to
each request the app makes, and the conversions row the app asks for.

-}

import Api.Data
import Dict
import Effect.Api
import Expect
import Json.Decode
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Route.Pathfinder as Route
import Set
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)



-- RECORDED ANSWERS (adapter var/bridge-maps/v2/api, gs-rest /txs live)


{-| ac-eth-send-pol: native ETH deposited on eth, filled as WETH (`_T111`) on pol.
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


decode : String -> List Api.Data.ExternalConversion
decode =
    Json.Decode.decodeString Api.Data.externalConversionDecoder
        >> Result.toMaybe
        >> Maybe.map List.singleton
        >> Maybe.withDefault []


tx : String -> String -> Maybe Bool -> String -> String -> Api.Data.Tx
tx network identifier isExternal from to =
    Api.Data.TxTxAccount
        { contractCreation = Just False
        , currency = network
        , fee = Nothing
        , fromAddress = from
        , height = 1
        , identifier = identifier
        , isExternal = isExternal
        , network = network
        , timestamp = 1730893259
        , toAddress = to
        , tokenTxId = Nothing
        , txHash = baseHash identifier
        , txType = "account"
        , value = { fiatValues = [], value = 1000 }
        }


baseHash : String -> String
baseHash =
    String.split "_" >> List.head >> Maybe.withDefault ""


spokePool : String
spokePool =
    "0x5c7bcd6e7de5423a257d81b442095a1a6ced35c5"


{-| `/eth/txs/16f7…` live: the root trace is `_I412`.
-}
ethDeposit : Api.Data.Tx
ethDeposit =
    tx "eth" "16f7467cb241ae6e9f82a71600e969e1677658603e45b576bfc964b7df6973af_I412" (Just True) "0xfee1a21c1373577c08b49d543835e0346b138cae" "0xfc99f58a8974a4bc36e60e2d490bb8d72899ee9f"


{-| `/eth/txs/16f7…_I413` live: an internal transfer of the same tx, NOT the root.
-}
ethDepositInternal : Api.Data.Tx
ethDepositInternal =
    tx "eth" "16f7467cb241ae6e9f82a71600e969e1677658603e45b576bfc964b7df6973af_I413" (Just False) "0xfc99f58a8974a4bc36e60e2d490bb8d72899ee9f" spokePool


polFill : Api.Data.Tx
polFill =
    tx "pol" "8c17c14e46f02badaf16816b5026425f4a9c11ea70acca7e9d5edebc9d79ae78_T111" Nothing spokePool "0x89f423567c2648bb828c3997f60c47b54f57fa6e"


{-| `/base/txs/2c1c…` live: `_I0`.
-}
baseDeposit : Api.Data.Tx
baseDeposit =
    tx "base" "2c1cfd091e7f5f56854abf49d275cac5ec7eaba29aad86ea7e381eb1b1ef2542_I0" (Just True) "0xa9c9259ca0efe93e2af7bda7d8f438683d4b1fd6" spokePool


{-| `/eth/txs/a89e…` live: `_I573`.
-}
ethFill : Api.Data.Tx
ethFill =
    tx "eth" "a89e63d97f3add6174a939845fdb8078da3f0e4a17c830b9f291176efcbddcb8_I573" (Just True) spokePool "0xa9c9259ca0efe93e2af7bda7d8f438683d4b1fd6"



-- THE API


idOf : Api.Data.Tx -> Id
idOf t =
    case t of
        Api.Data.TxTxAccount a ->
            ( a.network, a.identifier )

        Api.Data.TxTxUtxo u ->
            ( u.currency, u.txHash )


{-| What `/{network}/txs/{txHash}` answers: the tx by its identifier, or for a
bare hash its root transfer.
-}
served : List Api.Data.Tx -> String -> String -> Maybe Api.Data.Tx
served txs network txHash =
    let
        h =
            if String.startsWith "0x" txHash then
                String.dropLeft 2 txHash

            else
                txHash

        isRoot t =
            case t of
                Api.Data.TxTxAccount a ->
                    a.isExternal == Just True

                Api.Data.TxTxUtxo _ ->
                    True
    in
    txs
        |> List.filter (\t -> Tuple.first (idOf t) == network)
        |> List.filter (\t -> Tuple.second (idOf t) == h || (baseHash (Tuple.second (idOf t)) == h && isRoot t))
        |> List.head


{-| Answers the tx and conversion-leg requests of the last step.
-}
answer : List Api.Data.Tx -> App -> App
answer txs =
    App.respond
        (\eff ->
            case eff of
                Effect.Api.GetTxEffect { currency, txHash } toMsg ->
                    served txs currency txHash |> Maybe.map toMsg

                Effect.Api.GetConversionLegEffect { currency, txHash } toMsg ->
                    served txs currency txHash |> Maybe.map toMsg

                _ ->
                    Nothing
        )


{-| The tx opened by its deep link, by bare hash, as the URL route does.
-}
opened : List Api.Data.Tx -> String -> String -> App
opened txs network hash =
    Route.txRoute { network = network, txHash = hash }
        |> App.initAt
        |> answer txs


{-| `/{network}/txs/{hash}/conversions` of the opened tx answered `conversions`.
-}
gotConversions : Api.Data.Tx -> List Api.Data.ExternalConversion -> App -> App
gotConversions t conversions app =
    Dict.get (idOf t) (App.model app).network.txs
        |> Maybe.map (\node -> App.step (BrowserGotConversions Set.empty node conversions) app)
        |> Maybe.withDefault app


requested : App -> List ( String, String )
requested =
    App.apiEffects
        >> List.filterMap
            (\eff ->
                case eff of
                    Effect.Api.GetConversionLegEffect { currency, txHash } _ ->
                        Just ( currency, txHash )

                    _ ->
                        Nothing
            )


{-| Answers leg requests until none is left (at most a few rounds), keeping
the last non-empty request list for inspection.
-}
settle : List Api.Data.Tx -> App -> App
settle txs app =
    settleN 4 txs app


settleN : Int -> List Api.Data.Tx -> App -> App
settleN k txs app =
    if k <= 0 || List.isEmpty (requested app) then
        app

    else
        settleN (k - 1) txs (answer txs app)


edges : App -> List ( Id, Id )
edges =
    App.model >> .network >> .conversions >> Dict.keys


{-| Every leg request the flow makes, round by round, up to the first round
that asks for nothing.
-}
legRequests : List Api.Data.Tx -> App -> List ( String, String )
legRequests txs app =
    legRequestsN 4 txs app


legRequestsN : Int -> List Api.Data.Tx -> App -> List ( String, String )
legRequestsN k txs app =
    case requested app of
        [] ->
            []

        reqs ->
            if k <= 0 then
                reqs

            else
                reqs ++ legRequestsN (k - 1) txs (answer txs app)


sendPolApi : List Api.Data.Tx
sendPolApi =
    [ ethDeposit, ethDepositInternal, polFill ]


fillFromBaseApi : List Api.Data.Tx
fillFromBaseApi =
    [ baseDeposit, ethFill ]


suite : Test
suite =
    describe "a bare-hash bridge leg is the tx's root transfer, whatever its trace index"
        [ describe "ac-eth-send-pol opened at /pathfinder/eth/tx/0x16f7…"
            [ test "the deposit node is the root transfer `_I412`" <|
                \_ ->
                    opened sendPolApi "eth" "0x16f7467cb241ae6e9f82a71600e969e1677658603e45b576bfc964b7df6973af"
                        |> App.model
                        |> .network
                        |> .txs
                        |> Dict.member (idOf ethDeposit)
                        |> Expect.equal True
            , test "its conversions ask pol for the fill" <|
                \_ ->
                    opened sendPolApi "eth" "0x16f7467cb241ae6e9f82a71600e969e1677658603e45b576bfc964b7df6973af"
                        |> gotConversions ethDeposit (decode acrossEthSendPol)
                        |> legRequests sendPolApi
                        |> List.member ( "pol", "8c17c14e46f02badaf16816b5026425f4a9c11ea70acca7e9d5edebc9d79ae78_T111" )
                        |> Expect.equal True
            , test "the Bridge TX edge runs from the deposit root to the fill" <|
                \_ ->
                    opened sendPolApi "eth" "0x16f7467cb241ae6e9f82a71600e969e1677658603e45b576bfc964b7df6973af"
                        |> gotConversions ethDeposit (decode acrossEthSendPol)
                        |> settle sendPolApi
                        |> edges
                        |> Expect.equal [ ( idOf ethDeposit, idOf polFill ) ]
            , test "opened at the fill, the edge joins the same deposit root" <|
                \_ ->
                    opened sendPolApi "pol" "8c17c14e46f02badaf16816b5026425f4a9c11ea70acca7e9d5edebc9d79ae78_T111"
                        |> gotConversions polFill (decode acrossEthSendPol)
                        |> settle sendPolApi
                        |> edges
                        |> Expect.equal [ ( idOf ethDeposit, idOf polFill ) ]
            , test "an internal transfer of the deposit tx is no leg of it" <|
                \_ ->
                    opened sendPolApi "eth" "0x16f7467cb241ae6e9f82a71600e969e1677658603e45b576bfc964b7df6973af_I413"
                        |> gotConversions ethDepositInternal (decode acrossEthSendPol)
                        |> settle sendPolApi
                        |> edges
                        |> List.member ( idOf ethDepositInternal, idOf polFill )
                        |> Expect.equal False
            ]
        , describe "ac-eth-fill-from-base (eth fill is `_I573`, base deposit `_I0`)"
            [ test "opened at the eth fill, the edge runs from the base deposit" <|
                \_ ->
                    opened fillFromBaseApi "eth" "0xa89e63d97f3add6174a939845fdb8078da3f0e4a17c830b9f291176efcbddcb8"
                        |> gotConversions ethFill (decode acrossEthFillFromBase)
                        |> settle fillFromBaseApi
                        |> edges
                        |> Expect.equal [ ( idOf baseDeposit, idOf ethFill ) ]
            , test "opened at the eth fill, only base is asked" <|
                \_ ->
                    opened fillFromBaseApi "eth" "0xa89e63d97f3add6174a939845fdb8078da3f0e4a17c830b9f291176efcbddcb8"
                        |> gotConversions ethFill (decode acrossEthFillFromBase)
                        |> legRequests fillFromBaseApi
                        |> Expect.equal [ ( "base", "2c1cfd091e7f5f56854abf49d275cac5ec7eaba29aad86ea7e381eb1b1ef2542" ) ]
            , test "opened at the base deposit, the edge reaches the eth fill" <|
                \_ ->
                    opened fillFromBaseApi "base" "0x2c1cfd091e7f5f56854abf49d275cac5ec7eaba29aad86ea7e381eb1b1ef2542"
                        |> gotConversions baseDeposit (decode acrossEthFillFromBase)
                        |> settle fillFromBaseApi
                        |> edges
                        |> Expect.equal [ ( idOf baseDeposit, idOf ethFill ) ]
            ]
        ]
