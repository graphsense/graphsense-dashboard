module Scenario.ConversionLayoutTest exposing (suite)

{-| Once both legs of a swap or bridge are on the graph, they are laid out
(`InternalConversionLoopAddressesLoaded`): a bridge left to right in the order
the money moves, a swap, one owner's round trip, as a U-turn. Nodes a `.gs`
file put somewhere must keep their saved position, and so must an address
another tx on the graph has; only what the UI adds by itself is arranged.
-}

import Animation
import Api.Data
import Dict
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Json.Decode
import Maybe.Extra
import Model.Pathfinder exposing (Model)
import Model.Pathfinder.Deserialize exposing (Deserialized, DeserializedThing)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (FindPosition(..))
import Msg.Pathfinder exposing (Msg(..))
import Set
import Support.App as App exposing (App)
import Support.Env as Env
import Test exposing (Test, describe, test)
import Update.Pathfinder
import Update.Pathfinder.Network



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
                    |> Update.Pathfinder.fromDeserialized Env.updateConfig deserialized
                    |> Tuple.first
            )
        |> App.steps
            ([ ( ethTxId, ethHash, legs.eth ), ( btcTxId, btcHash, legs.btc ) ]
                |> List.filterMap bulk
            )


{-| Answers the conversion lookups, then the lookup of a leg the graph lacks.
Returns the `BrowserGotConversions` answers, which carry the set of nodes to
leave alone into the layout, and the `BrowserGotConversionLoop`s the leg
lookups produce. A leg already on the graph is paired right away, without a
lookup.
-}
resolveConversion : Legs -> App -> ( App, List Msg, List Msg )
resolveConversion legs app =
    let
        answers =
            App.apiEffects app
                |> List.filterMap
                    (\eff ->
                        case eff of
                            Effect.Api.GetConversionEffect _ toMsg ->
                                Just (toMsg [ conversion ])

                            _ ->
                                Nothing
                    )

        withConversions =
            App.steps answers app

        loops =
            App.apiEffects withConversions
                |> List.filterMap
                    (\eff ->
                        case eff of
                            Effect.Api.GetConversionLegEffect { txHash } toMsg ->
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
    ( withConversions, answers, loops )


{-| What the eventual message fires once the addresses are loaded. It goes out
as a `Cmd`, which the harness cannot follow, so it is rebuilt from the loop
message that registered it, or, for a leg paired without a lookup, from the
conversions answer whose set that loop is handed.
-}
arrangeFor : Msg -> List Msg
arrangeFor msg =
    case msg of
        BrowserGotConversionLoop keep _ conv _ ->
            [ InternalConversionLoopAddressesLoaded keep conv ]

        BrowserGotConversions keep _ convs ->
            List.map (InternalConversionLoopAddressesLoaded keep) convs

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


{-| The file's graph, with the conversion looked up the way the UI does it,
with no nodes pinned, after `prepare` changed the model.
-}
expandInUi : Legs -> Api.Data.ExternalConversion -> (Model -> Model) -> App
expandInUi legs conv prepare =
    let
        opened =
            open legs bothLegs
                |> App.mapModel prepare

        lookUpConversions =
            Dict.get btcTxId (App.model opened).network.txs
                |> Maybe.map (\tx -> [ BrowserGotConversions Set.empty tx [ conv ] ])
                |> Maybe.withDefault []
    in
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
        |> App.step (InternalConversionLoopAddressesLoaded Set.empty conv)


suite : Test
suite =
    describe "conversion layout"
        [ test "a file's nodes keep their saved positions, and the edge is drawn" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            ( app, answers, loops ) =
                                open legs bothLegs
                                    |> resolveConversion legs

                            arranged =
                                app
                                    |> App.steps loops
                                    |> App.steps (List.concatMap arrangeFor answers)

                            positions a =
                                ( List.map (\t -> addressAt t.id a) bothLegs.addresses
                                , List.map (\t -> txAt t.id a) bothLegs.txs
                                )
                        in
                        Expect.all
                            [ \_ -> List.length answers |> Expect.greaterThan 0
                            , \a ->
                                positions a
                                    |> Expect.equal
                                        ( List.map (\t -> Just ( t.x, t.y )) bothLegs.addresses
                                        , List.map (\t -> Just ( t.x, t.y )) bothLegs.txs
                                        )
                            , \a -> Dict.keys (App.model a).network.conversions |> Expect.equal [ ( ethTxId, btcTxId ) ]
                            ]
                            arranged
        , test "a leg missing from the file is added left of the one from the file" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            btcOnly =
                                { bothLegs
                                    | addresses = List.filter (\t -> Tuple.first t.id == "btc") bothLegs.addresses
                                    , txs = List.filter (\t -> t.id == btcTxId) bothLegs.txs
                                }

                            ( app, _, loops ) =
                                open legs btcOnly
                                    |> resolveConversion legs

                            arranged =
                                app
                                    |> App.steps loops
                                    |> answerAddresses legs
                                    |> App.steps (List.concatMap arrangeFor loops)

                            ( btcX, btcY ) =
                                txAt btcTxId arranged |> Maybe.withDefault ( 0, 0 )
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
                                [ -- the new leg goes left of the one from the
                                  -- file, on its row: sender, tx, vault
                                  Just ( btcX - 16, btcY )
                                , Just ( btcX - 20, btcY )
                                , Just ( btcX - 12, btcY )

                                -- the file's nodes stay put
                                , Just ( 2, 11 )
                                , Just ( 30, 30 )
                                , Just ( 40, 1.3 )
                                ]
        , test "expanding a bridge in the UI lays it out left to right, stacking a leg's inputs" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            arranged =
                                expandInUi legs conversion identity

                            -- the whole graph is snapped to the grid, txs included
                            ( ( ethX, ethY ), ( btcX, btcY ) ) =
                                ( txAt ethTxId arranged |> Maybe.withDefault ( 0, 0 )
                                , txAt btcTxId arranged |> Maybe.withDefault ( 0, 0 )
                                )
                        in
                        ( ( btcX - ethX, btcY - ethY )
                        , [ ethSender, ethVault, btcInput1, btcInput2, btcRecipient ]
                            |> List.map (\id -> addressAt id arranged)
                        )
                            |> Expect.equal
                                ( -- the output leg right of the input leg
                                  ( 16, 0 )
                                , [ -- input leg: sender left, vault right
                                    Just ( ethX - 4, ethY )
                                  , Just ( ethX + 4, ethY )

                                  -- output leg: inputs left, one below the
                                  -- other, recipient right
                                  , Just ( btcX - 4, btcY )
                                  , Just ( btcX - 4, btcY + 2.5 )
                                  , Just ( btcX + 4, btcY )
                                  ]
                                )
        , test "an address another tx has stays put, and the bridge hangs off it" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            -- the tx the user opened paid the bridge's sender
                            otherTx =
                                case legs.eth of
                                    Api.Data.TxTxAccount t ->
                                        Api.Data.TxTxAccount
                                            { t
                                                | txHash = String.repeat 32 "cd"
                                                , identifier = String.repeat 32 "cd"
                                                , fromAddress = "0x" ++ String.repeat 40 "3"
                                                , toAddress = Tuple.second ethSender
                                            }

                                    other ->
                                        other

                            -- without snapping, which would move every node
                            arranged =
                                expandInUi legs
                                    conversion
                                    (\m ->
                                        let
                                            config =
                                                m.config
                                        in
                                        { m
                                            | config = { config | snapToGrid = False }
                                            , network =
                                                Update.Pathfinder.Network.addTx m.config otherTx m.network
                                                    |> Tuple.second
                                        }
                                    )

                            sender =
                                addressAt ethSender arranged |> Maybe.withDefault ( 0, 0 )
                        in
                        ( addressAt ethSender arranged == Just ( -13, 7.1 )
                        , txAt ethTxId arranged
                        , addressAt ethVault arranged
                        )
                            |> Expect.equal
                                ( True
                                , Just ( Tuple.first sender + 4, Tuple.second sender )
                                , Just ( Tuple.first sender + 8, Tuple.second sender )
                                )
        , test "a swap keeps its U-turn" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            arranged =
                                expandInUi legs { conversion | conversionType = Api.Data.ExternalConversionConversionTypeDexSwap } identity

                            ( btcX, btcY ) =
                                txAt btcTxId arranged |> Maybe.withDefault ( 0, 0 )
                        in
                        [ btcRecipient, btcInput1, btcInput2 ]
                            |> List.map (\id -> addressAt id arranged)
                            |> Expect.equal
                                [ -- output leg, mirrored: recipient left,
                                  -- inputs right and one below the other
                                  Just ( btcX - 4, btcY )
                                , Just ( btcX + 4, btcY )
                                , Just ( btcX + 4, btcY + 2.5 )
                                ]
        , test "a swap's address does not land on a node already there" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            swap =
                                { conversion | conversionType = Api.Data.ExternalConversionConversionTypeDexSwap }

                            -- where the U-turn puts the leg's first input:
                            -- right of the leg, on its row (the file's btc tx
                            -- is at ( 9, 17.2 ))
                            blocker =
                                ( "btc", "blocker" )

                            arranged =
                                expandInUi legs
                                    swap
                                    (\m ->
                                        let
                                            config =
                                                m.config
                                        in
                                        { m
                                            | config = { config | snapToGrid = False }
                                            , network =
                                                Update.Pathfinder.Network.addAddressWithPosition m.config (Fixed 13 17.2) blocker m.network
                                                    |> Tuple.second
                                        }
                                    )

                            ( btcX, btcY ) =
                                txAt btcTxId arranged |> Maybe.withDefault ( 0, 0 )
                        in
                        [ blocker, btcInput1, btcInput2 ]
                            |> List.map (\id -> addressAt id arranged)
                            |> Expect.equal
                                [ Just ( 13, 17.2 )

                                -- each goes one row further down instead
                                , Just ( btcX + 4, btcY + 2.5 )
                                , Just ( btcX + 4, btcY + 5 )
                                ]
        , test "a bridge goes down to the first row where nothing is in its way" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            -- the chain hangs off the eth leg at ( 5, -5.3 ):
                            -- the btc leg goes to x 21, its recipient to x 25
                            blocker =
                                ( "btc", "blocker" )

                            arranged =
                                expandInUi legs
                                    conversion
                                    (\m ->
                                        let
                                            config =
                                                m.config
                                        in
                                        { m
                                            | config = { config | snapToGrid = False }
                                            , network =
                                                Update.Pathfinder.Network.addAddressWithPosition m.config (Fixed 25 -5.3) blocker m.network
                                                    |> Tuple.second
                                        }
                                    )

                            network =
                                (App.model arranged).network

                            others id =
                                (network.addresses |> Dict.toList |> List.map (\( i, a ) -> ( i, ( a.x, Animation.getTo a.y ) )))
                                    ++ (network.txs |> Dict.toList |> List.map (\( i, t ) -> ( i, ( t.x, Animation.getTo t.y ) )))
                                    |> List.filter (Tuple.first >> (/=) id)
                                    |> List.map Tuple.second

                            -- closer than a node and its label take
                            clashes id =
                                Maybe.map
                                    (\( x, y ) -> List.any (\( ox, oy ) -> abs (ox - x) < 3 && abs (oy - y) < 2) (others id))
                                    (Maybe.Extra.or (addressAt id arranged) (txAt id arranged))
                                    |> Maybe.withDefault True
                        in
                        ( ( addressAt blocker arranged, txAt ethTxId arranged, txAt btcTxId arranged )
                        , List.filter clashes [ ethTxId, btcTxId, ethSender, ethVault, btcInput1, btcInput2, btcRecipient ]
                        )
                            |> Expect.equal
                                ( ( -- the blocker stays, the chain is one row down
                                    Just ( 25, -5.3 )
                                  , Just ( 5, -2.8 )
                                  , Just ( 21, -2.8 )
                                  )
                                , []
                                )
        , test "the address the user started from stays put, and the bridge ends at it" <|
            \_ ->
                withLegs <|
                    \legs ->
                        let
                            arranged =
                                expandInUi legs
                                    conversion
                                    (\m ->
                                        let
                                            config =
                                                m.config
                                        in
                                        { m
                                            | config = { config | snapToGrid = False }
                                            , network =
                                                Update.Pathfinder.Network.updateAddress btcRecipient (\a -> { a | isStartingPoint = True }) m.network
                                        }
                                    )

                            recipient =
                                addressAt btcRecipient arranged |> Maybe.withDefault ( 0, 0 )
                        in
                        ( addressAt btcRecipient arranged, txAt btcTxId arranged, txAt ethTxId arranged )
                            |> Expect.equal
                                ( -- where the file put it
                                  Just ( 40, 1.3 )
                                , Just ( Tuple.first recipient - 4, Tuple.second recipient )
                                , Just ( Tuple.first recipient - 20, Tuple.second recipient )
                                )
        ]
