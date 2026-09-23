module Scenario.ConversionLayoutTest exposing (suite)

{-| Once both legs of a swap or bridge are on the graph, the addresses of the
two legs are laid out as a U-turn (`InternalConversionLoopAddressesLoaded`).
Nodes a `.gs` file put somewhere must keep their saved position; only what the
UI adds by itself is arranged.
-}

import Animation
import Api.Data
import Dict
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Json.Decode
import Model.Pathfinder.Deserialize exposing (Deserialized, DeserializedThing)
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Set
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Update.Pathfinder



-- A bridge from an ETH transfer to a BTC payout.


ethHash : String
ethHash =
    String.repeat 32 "ab"


btcHash : String
btcHash =
    "04d92601677d62a985310b61a301e74870fa942c8be0648e16b1db23b996a8cd"


ethTxId : Id
ethTxId =
    ( "eth", ethHash )


btcTxId : Id
btcTxId =
    ( "btc", btcHash )


ethSender : Id
ethSender =
    ( "eth", "0x" ++ String.repeat 40 "1" )


ethVault : Id
ethVault =
    ( "eth", "0x" ++ String.repeat 40 "2" )


{-| The two inputs of the BTC leg, as in the `tx_utxo` fixture.
-}
btcInput1 : Id
btcInput1 =
    ( "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" )


btcInput2 : Id
btcInput2 =
    ( "btc", "addressB" )


btcRecipient : Id
btcRecipient =
    ( "btc", "addressC" )


conversion : Api.Data.ExternalConversion
conversion =
    { conversionType = Api.Data.ExternalConversionConversionTypeBridgeTx
    , fromAddress = Tuple.second ethSender
    , fromAmount = "1"
    , fromAsset = "eth"
    , fromAssetTransfer = ethHash
    , fromIsSupportedAsset = True
    , fromNetwork = "eth"
    , toAddress = Tuple.second btcRecipient
    , toAmount = "1"
    , toAsset = "btc"
    , toAssetTransfer = btcHash
    , toIsSupportedAsset = True
    , toNetwork = "btc"
    }


type alias Legs =
    { eth : Api.Data.Tx
    , btc : Api.Data.Tx
    , address : Api.Data.Address
    }


withLegs : (Legs -> Expectation) -> Expectation
withLegs f =
    Result.map3
        (\eth btc address ->
            f
                { eth =
                    Api.Data.TxTxAccount
                        { eth
                            | txHash = ethHash
                            , identifier = ethHash
                            , fromAddress = Tuple.second ethSender
                            , toAddress = Tuple.second ethVault
                        }
                , btc = btc
                , address = address
                }
        )
        (Json.Decode.decodeString Api.Data.txAccountDecoder Fixture.txAccount)
        (Json.Decode.decodeString Api.Data.txDecoder Fixture.txUtxo)
        (Json.Decode.decodeString Api.Data.addressDecoder Fixture.address)
        |> Result.mapError Json.Decode.errorToString
        |> (\r ->
                case r of
                    Ok e ->
                        e

                    Err e ->
                        Expect.fail e
           )


thing : Int -> Id -> ( Float, Float ) -> DeserializedThing
thing index id ( x, y ) =
    { id = id, x = x, y = y, isStartingPoint = False, index = index }


{-| Opens `deserialized` with grid snapping on, and answers its bulk tx
requests with whichever legs it contains.
-}
open : Legs -> Deserialized -> App
open legs deserialized =
    let
        deserializing =
            { deserialized = deserialized, addresses = [], txs = [] }

        bulk ( id, hash, tx ) =
            if List.any (.id >> (==) id) deserialized.txs then
                Just (BrowserGotBulkTxs deserializing [ ( hash, tx ) ])

            else
                Nothing
    in
    App.init
        |> App.mapModel
            (\m ->
                let
                    config =
                        m.config
                in
                { m | config = { config | snapToGrid = True } }
                    |> Update.Pathfinder.fromDeserialized deserialized
                    |> Tuple.first
            )
        |> App.steps
            ([ ( ethTxId, ethHash, legs.eth ), ( btcTxId, btcHash, legs.btc ) ]
                |> List.filterMap bulk
            )


{-| Answers the conversion lookups, then the lookup of the other leg, and
returns the `BrowserGotConversionLoop` that produces -- the step that carries
the set of nodes to leave alone into the layout.
-}
resolveConversion : Legs -> App -> ( App, List Msg )
resolveConversion legs app =
    let
        withConversions =
            App.respond
                (\eff ->
                    case eff of
                        Effect.Api.GetConversionEffect _ toMsg ->
                            Just (toMsg [ conversion ])

                        _ ->
                            Nothing
                )
                app

        loops =
            App.apiEffects withConversions
                |> List.filterMap
                    (\eff ->
                        case eff of
                            Effect.Api.GetTxEffect { txHash } toMsg ->
                                if txHash == ethHash then
                                    Just (toMsg legs.eth)

                                else if txHash == btcHash then
                                    Just (toMsg legs.btc)

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
    in
    ( withConversions, loops )


{-| What the eventual message fires once the addresses are loaded. It goes out
as a `Cmd`, which the harness cannot follow, so it is rebuilt from the loop
message that registered it.
-}
arrangeFor : Msg -> List Msg
arrangeFor msg =
    case msg of
        BrowserGotConversionLoop keep _ conv _ ->
            [ InternalConversionLoopAddressesLoaded keep conv ]

        _ ->
            []


{-| Loads a leg's missing addresses, answering every address request.
-}
answerAddresses : Legs -> App -> App
answerAddresses legs =
    let
        address =
            legs.address
    in
    App.respond
        (\eff ->
            case eff of
                Effect.Api.GetAddressEffect req toMsg ->
                    Just (toMsg { address | currency = req.currency, address = req.address })

                _ ->
                    Nothing
        )


addressAt : Id -> App -> Maybe ( Float, Float )
addressAt id app =
    Dict.get id (App.model app).network.addresses
        |> Maybe.map (\a -> ( a.x, Animation.getTo a.y ))


txAt : Id -> App -> Maybe ( Float, Float )
txAt id app =
    Dict.get id (App.model app).network.txs
        |> Maybe.map (\t -> ( t.x, Animation.getTo t.y ))


{-| Both legs and all their addresses, at positions no layout would produce and
off the grid.
-}
bothLegs : Deserialized
bothLegs =
    { name = "bridge"
    , addresses =
        [ thing 1 ethSender ( -13, 7.1 )
        , thing 2 ethVault ( 21, -3 )
        , thing 3 btcInput1 ( 2, 11 )
        , thing 4 btcInput2 ( 30, 30 )
        , thing 5 btcRecipient ( 40, 1.3 )
        ]
    , txs =
        [ thing 6 ethTxId ( 5, -5.3 )
        , thing 7 btcTxId ( 9, 17.2 )
        ]
    , annotations = []
    , aggEdges = []
    }


suite : Test
suite =
    describe "conversion layout"
        [ test "a file's nodes keep their saved positions, and the edge is drawn" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            ( app, loops ) =
                                open legs bothLegs
                                    |> resolveConversion legs

                            arranged =
                                app
                                    |> App.steps loops
                                    |> App.steps (List.concatMap arrangeFor loops)

                            positions a =
                                ( List.map (\t -> addressAt t.id a) bothLegs.addresses
                                , List.map (\t -> txAt t.id a) bothLegs.txs
                                )
                        in
                        Expect.all
                            [ \_ -> List.length loops |> Expect.greaterThan 0
                            , \a ->
                                positions a
                                    |> Expect.equal
                                        ( List.map (\t -> Just ( t.x, t.y )) bothLegs.addresses
                                        , List.map (\t -> Just ( t.x, t.y )) bothLegs.txs
                                        )
                            , \a -> Dict.keys (App.model a).network.conversions |> Expect.equal [ ( ethTxId, btcTxId ) ]
                            ]
                            arranged
        , test "a leg missing from the file is added and laid out as a U-turn" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            btcOnly =
                                { bothLegs
                                    | addresses = List.filter (\t -> Tuple.first t.id == "btc") bothLegs.addresses
                                    , txs = List.filter (\t -> t.id == btcTxId) bothLegs.txs
                                }

                            ( app, loops ) =
                                open legs btcOnly
                                    |> resolveConversion legs

                            arranged =
                                app
                                    |> App.steps loops
                                    |> answerAddresses legs
                                    |> App.steps (List.concatMap arrangeFor loops)
                        in
                        [ ethTxId, ethSender, ethVault, btcInput1, btcInput2, btcRecipient ]
                            |> List.map
                                (\id ->
                                    if id == ethTxId then
                                        txAt id arranged

                                    else
                                        addressAt id arranged
                                )
                            |> Expect.equal
                                [ -- the new leg goes below the one from the file
                                  Just ( 9, 19.2 )
                                , Just ( 5, 19.2 )
                                , Just ( 13, 19.2 )

                                -- the file's nodes stay put
                                , Just ( 2, 11 )
                                , Just ( 30, 30 )
                                , Just ( 40, 1.3 )
                                ]
        , test "expanding a conversion in the UI lays it out as a U-turn, stacking a leg's inputs" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            -- The same graph, but the conversion is looked up
                            -- the way the UI does it, with no nodes pinned.
                            opened =
                                open legs bothLegs

                            lookUpConversions =
                                Dict.get btcTxId (App.model opened).network.txs
                                    |> Maybe.map (\tx -> [ BrowserGotConversions Set.empty tx [ conversion ] ])
                                    |> Maybe.withDefault []

                            arranged =
                                opened
                                    |> App.steps lookUpConversions
                                    |> App.respond
                                        (\eff ->
                                            case eff of
                                                Effect.Api.GetTxEffect _ toMsg ->
                                                    Just (toMsg legs.eth)

                                                _ ->
                                                    Nothing
                                        )
                                    |> App.step (InternalConversionLoopAddressesLoaded Set.empty conversion)

                            -- the whole graph is snapped to the grid, txs included
                            ( ( ethX, ethY ), ( btcX, btcY ) ) =
                                ( txAt ethTxId arranged |> Maybe.withDefault ( 0, 0 )
                                , txAt btcTxId arranged |> Maybe.withDefault ( 0, 0 )
                                )
                        in
                        [ ethSender, ethVault, btcRecipient, btcInput1, btcInput2 ]
                            |> List.map (\id -> addressAt id arranged)
                            |> Expect.equal
                                [ -- input leg: sender left, vault right
                                  Just ( ethX - 4, ethY )
                                , Just ( ethX + 4, ethY )

                                -- output leg, mirrored: recipient left, inputs
                                -- right and one below the other
                                , Just ( btcX - 4, btcY )
                                , Just ( btcX + 4, btcY )
                                , Just ( btcX + 4, btcY + 2.5 )
                                ]
        ]
