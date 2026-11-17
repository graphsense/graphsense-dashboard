module Scenario.BridgeNotesTest exposing (suite)

{-| A conversion the graph cannot draw (an unsupported swap or a half bridge)
becomes a note in the tx's side panel. A complete bridge row is drawn even with
a token outside the registry (Scenario.BridgeUnsupportedAssetsTest), so the
bridge rows here are made halves, their payout still pending. Every such row gets its own note naming
its own target, and the bridge when the row names its `protocol`; only
identical notes are shown once.

The conversions are the adapter's recorded answers (var/bridge-maps/v2/api).

-}

import Api.Data
import Dict exposing (Dict)
import Effect.Api
import Expect
import Html.Attributes
import Html.Styled
import Json.Decode
import Model.Locale exposing (State(..))
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Model
import RecordSetter exposing (s_locale, s_mapping)
import Route.Pathfinder as Route
import Set
import Support.App as App exposing (App)
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import View.Pathfinder



-- RECORDED ANSWERS


{-| wh-base-redeem-resend, as base answers it: a Wormhole redeem from bnb and,
in the same tx, a re-send back to bnb. Both carry unregistered tokens.
-}
redeem : String
redeem =
    """
    {
      "conversion_type": "bridge_tx",
      "from_address": "0x7b8460883aabf656feeb8af11a921d0002ecc43d",
      "to_address": "0x346ec0fadbf97cc3a5e2fbdaa7e4d10503f1052e",
      "from_asset": "0xc3e356c4f0ee6e93b0637811b1a30f7c43fc8e61",
      "to_asset": "0x51436f6bd047797de7d11e9d32685f029aed1069",
      "from_amount": "0x3503441abe",
      "to_amount": "0x3503441abe",
      "from_asset_transfer": "65daa706aef8793a40ef105f2cb06007046cc751ca8fc968c377849835804d2e_T8",
      "to_asset_transfer": "63ec822c051c7ec0433b21e596c58206bc170cc56e1a620ba74bd12a6ae84d54_T1",
      "from_network": "bnb",
      "to_network": "base",
      "from_is_supported_asset": false,
      "to_is_supported_asset": false,
      "protocol": "wormhole",
      "transfer_id": "4/000000000000000000000000b6f6d86a8f9879a9c87f643768d9efc38c1da6e7/896510"
    }
    """


resend : String
resend =
    """
    {
      "conversion_type": "bridge_tx",
      "from_address": "0xb2efa3d65c2d12c3e8ce7dbcfc0dae62289fb4c2",
      "to_address": "0x346ec0fadbf97cc3a5e2fbdaa7e4d10503f1052e",
      "from_asset": "0x51436f6bd047797de7d11e9d32685f029aed1069",
      "to_asset": "0xc3e356c4f0ee6e93b0637811b1a30f7c43fc8e61",
      "from_amount": "0x3503441abe",
      "to_amount": "0x3503441abe",
      "from_asset_transfer": "63ec822c051c7ec0433b21e596c58206bc170cc56e1a620ba74bd12a6ae84d54_T5",
      "to_asset_transfer": "af566c117b2cc1c7617eeb31b3eb37ed1867fa83129bdb94b9d68df0e2c83b91_T44",
      "from_network": "base",
      "to_network": "bnb",
      "from_is_supported_asset": false,
      "to_is_supported_asset": false,
      "protocol": "wormhole",
      "transfer_id": "30/0000000000000000000000008d2de8d2f73f1f4cab472ac9a881c9b123c79627/285095"
    }
    """


{-| wh-eth-send-arb: a swap into ETH, then a Wormhole send of an unregistered
token to arb. Both rows are notes; the swap's used to hide the bridge's.
-}
swap : String
swap =
    """
    {
      "conversion_type": "dex_swap",
      "from_address": "0x000000000004444c5dc75cb358380d2e3de08a90",
      "to_address": "0x000000000004444c5dc75cb358380d2e3de08a90",
      "from_asset": "0x3010ccb5419f1ef26d40a7cd3f0d707a0fa127dc",
      "to_asset": "native",
      "from_amount": "0x16ff3019595ff7c5b1da",
      "to_amount": "0xf61b31aad74ec7",
      "from_asset_transfer": "0x5d3d5ceae42c16f11362ddcbead82ae7dbb9b3bfacec465fe6d806a86a4890fd_T102",
      "to_asset_transfer": "0x5d3d5ceae42c16f11362ddcbead82ae7dbb9b3bfacec465fe6d806a86a4890fd_I747",
      "from_network": "eth",
      "to_network": "eth",
      "from_is_supported_asset": false,
      "to_is_supported_asset": true
    }
    """


send : String
send =
    """
    {
      "conversion_type": "bridge_tx",
      "from_address": "0xd7ca08ec1aee9cce8a8eda9365343ef197674e1a",
      "to_address": "0xd7ca08ec1aee9cce8a8eda9365343ef197674e1a",
      "from_asset": "0x3010ccb5419f1ef26d40a7cd3f0d707a0fa127dc",
      "to_asset": "0x28bc3f91e153cffaf7f8c895784a8ea3e1730bf3",
      "from_amount": "0x16ff3019595ec8d6f800",
      "to_amount": "0x16ff3019595ec8d6f800",
      "from_asset_transfer": "5d3d5ceae42c16f11362ddcbead82ae7dbb9b3bfacec465fe6d806a86a4890fd_T104",
      "to_asset_transfer": "5c6c5a93e1c066edceb645b88ee2fef3d7338639cf021519189ea1b3e37bd3be_T1",
      "from_network": "eth",
      "to_network": "arb",
      "from_is_supported_asset": false,
      "to_is_supported_asset": false,
      "protocol": "wormhole",
      "transfer_id": "2/0000000000000000000000003ee18b2214aff97000d974cf647e7c347e8fa585/704561"
    }
    """


{-| Recorded rows as the client decodes them; a row that does not decode is
missing, which the first test reports.
-}
decode : List String -> List Api.Data.ExternalConversion
decode =
    List.filterMap (Json.Decode.decodeString Api.Data.externalConversionDecoder >> Result.toMaybe)



-- TXS


tx : String -> String -> Api.Data.Tx
tx network identifier =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = network
        , fee = Nothing
        , fromAddress = "0x" ++ String.repeat 40 "1"
        , height = 1
        , identifier = identifier
        , isExternal = Nothing
        , network = network
        , timestamp = 1788254088
        , toAddress = "0x" ++ String.repeat 40 "2"
        , tokenTxId = Nothing
        , txHash = String.split "_" identifier |> List.head |> Maybe.withDefault identifier
        , txType = "account"
        , value = { fiatValues = [], value = 1000 }
        }


baseTx : Api.Data.Tx
baseTx =
    tx "base" "63ec822c051c7ec0433b21e596c58206bc170cc56e1a620ba74bd12a6ae84d54_I0"


ethTx : Api.Data.Tx
ethTx =
    tx "eth" "5d3d5ceae42c16f11362ddcbead82ae7dbb9b3bfacec465fe6d806a86a4890fd_I0"


idOf : Api.Data.Tx -> Id
idOf t =
    case t of
        Api.Data.TxTxAccount a ->
            ( a.network, a.identifier )

        Api.Data.TxTxUtxo u ->
            ( u.currency, u.txHash )



-- DRIVING


{-| The tx opened by its deep link (bare hash), answered with its root, and
then its conversions answered.
-}
withConversions : Api.Data.Tx -> List (List Api.Data.ExternalConversion) -> App
withConversions t answers =
    let
        ( network, identifier ) =
            idOf t

        opened =
            Route.txRoute { network = network, txHash = String.split "_" identifier |> List.head |> Maybe.withDefault identifier }
                |> App.initAt
                |> App.respond
                    (\eff ->
                        case eff of
                            Effect.Api.GetTxEffect _ toMsg ->
                                Just (toMsg t)

                            _ ->
                                Nothing
                    )
    in
    List.foldl
        (\conversions app ->
            Dict.get (idOf t) (App.model app).network.txs
                |> Maybe.map (\node -> App.step (BrowserGotConversions Set.empty node conversions) app)
                |> Maybe.withDefault app
        )
        opened
        answers


{-| The English texts of the notes, from `lang/en.yaml` (the test locale has
no translations loaded, so it would show the bare keys).
-}
english : Dict String String
english =
    Dict.fromList
        [ ( "unsupported-swap-notice", "Note: Unsupported swap transaction detected, target {0} ({1})" )
        , ( "unsupported-bridge-notice", "Note: Unsupported bridge transaction detected, target {0} ({1})" )
        , ( "unsupported-protocol-bridge-notice", "Note: Unsupported {2} bridge transaction detected, target {0} ({1})" )
        ]


{-| The notes the open tx panel shows, in English.
-}
notes : App -> Query.Multiple Msg
notes app =
    let
        vc =
            Env.viewConfig
                |> s_locale (Env.viewConfig.locale |> s_mapping (Settled english))
    in
    View.Pathfinder.view Plugin.Model.emptyModelState vc (App.model app)
        |> Html.Styled.div []
        |> Html.Styled.toUnstyled
        |> Query.fromHtml
        |> Query.findAll [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-conversion-notice") ]


{-| Exactly these notes, in this order.
-}
expectNotes : List String -> App -> Expect.Expectation
expectNotes expected app =
    let
        found =
            notes app
    in
    Expect.all
        ((\_ -> Query.count (Expect.equal (List.length expected)) found)
            :: List.indexedMap (\i text -> \_ -> found |> Query.index i |> Query.has [ Selector.text text ]) expected
        )
        ()


{-| The rows with their bridge payouts pending, as a half row names them.
-}
halfRows : List String -> List Api.Data.ExternalConversion
halfRows =
    decode
        >> List.map
            (\c ->
                if c.conversionType == Api.Data.ExternalConversionConversionTypeBridgeTx then
                    { c | toAssetTransfer = "pending" }

                else
                    c
            )


suite : Test
suite =
    describe "one note per undrawable conversion"
        [ test "the recorded answers decode" <|
            \_ ->
                decode [ redeem, resend, swap, send ]
                    |> List.length
                    |> Expect.equal 4
        , test "a redeem and a re-send in one tx get a note each (wh-base-redeem-resend)" <|
            \_ ->
                withConversions baseTx [ halfRows [ redeem, resend ] ]
                    |> expectNotes
                        [ "Note: Unsupported Wormhole bridge transaction detected, target 0x346ec0fadbf97cc3a5e2fbdaa7e4d10503f1052e (BASE)"
                        , "Note: Unsupported Wormhole bridge transaction detected, target 0x346ec0fadbf97cc3a5e2fbdaa7e4d10503f1052e (BNB)"
                        ]
        , test "a swap note no longer hides the bridge's (wh-eth-send-arb)" <|
            \_ ->
                withConversions ethTx [ halfRows [ swap, send ] ]
                    |> expectNotes
                        [ "Note: Unsupported swap transaction detected, target 0x000000000004444c5dc75cb358380d2e3de08a90 (ETH)"
                        , "Note: Unsupported Wormhole bridge transaction detected, target 0xd7ca08ec1aee9cce8a8eda9365343ef197674e1a (ARB)"
                        ]
        , test "the same answer twice shows each note once" <|
            \_ ->
                withConversions baseTx [ halfRows [ redeem, resend ], halfRows [ redeem, resend ] ]
                    |> notes
                    |> Query.count (Expect.equal 2)
        , test "rows arriving in separate answers keep the earlier notes" <|
            \_ ->
                withConversions ethTx [ halfRows [ swap ], halfRows [ send ] ]
                    |> notes
                    |> Query.count (Expect.equal 2)
        , test "a bridge row without a protocol keeps the plain note" <|
            \_ ->
                withConversions ethTx [ halfRows [ send ] |> List.map (\c -> { c | protocol = Nothing }) ]
                    |> expectNotes [ "Note: Unsupported bridge transaction detected, target 0xd7ca08ec1aee9cce8a8eda9365343ef197674e1a (ARB)" ]
        ]
