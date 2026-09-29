module Scenario.ConversionLegsTest exposing (suite)

{-| Pins which sub-transactions a swap edge is drawn between.

For a sub-tx id `/txs/{id}/conversions` returns the conversions naming it as a
leg; for the whole tx (bare hash or root `_I0`) it returns every conversion of
the tx, whose legs are other sub-txs, so the answered tx must never itself be
paired as a leg.

-}

import Api.Data
import Dict
import Effect.Api
import Effect.Pathfinder
import Expect
import Html.Attributes
import Http
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Id as Id
import Model
import Model.Dialog as Dialog
import Model.Notification as Notification
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Selection exposing (Selection(..))
import Msg.Pathfinder exposing (Msg(..), OutMsg(..))
import Set
import Support.App as App exposing (App)
import Support.MainApp as MainApp
import Support.SwapFixture exposing (accountTx, dexSwap, hash, inputLegId, outputLegId, settlement, swapper, txRequest)
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Update.Graph.Transform as Transform
import Update.Pathfinder.Network as Network
import Update.Statusbar as Statusbar
import Util exposing (removeLeading0x)



-- FIXTURE: one dex swap inside one bnb tx


sender : String
sender =
    "0x53227a6d5a129143b6ad760810ece62c79ab8e97"


rootId : String
rootId =
    hash ++ "_I0"


rootTx : Api.Data.Tx
rootTx =
    accountTx rootId sender settlement


inputLegTx : Api.Data.Tx
inputLegTx =
    accountTx inputLegId swapper settlement


outputLegTx : Api.Data.Tx
outputLegTx =
    accountTx outputLegId settlement swapper


{-| The same swap as the adapter serves it enriched: the native input leg
carries no display metadata, the token output leg carries its curated symbol
and decimals.
-}
curatedSwap : Api.Data.ExternalConversion
curatedSwap =
    { dexSwap
        | fromAsset = "native"
        , toAssetSymbol = Just "USD1"
        , toAssetDecimals = Just 18
    }


{-| A native leg the adapter synthesized (DASHBOARD\_CHANGES D-27): an opaque
id, fetched exactly as served; a 404 ends the walk.
-}
synthesizedLegId : String
synthesizedLegId =
    hash ++ "_I1048583"


synthesizedLegTx : Api.Data.Tx
synthesizedLegTx =
    accountTx synthesizedLegId swapper settlement


nativeInSwap : Api.Data.ExternalConversion
nativeInSwap =
    { dexSwap
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
    App.step (BrowserGotTx (txRequest (identifierOf tx)) tx)


{-| The API answered `/txs/{identifier}/conversions` with these conversions.
-}
gotConversionsFor : String -> List Api.Data.ExternalConversion -> App -> App
gotConversionsFor identifier conversions app =
    Dict.get (Id.init "bnb" identifier) (App.model app).network.txs
        |> Maybe.map (\tx -> App.step (BrowserGotConversions Set.empty tx conversions) app)
        |> Maybe.withDefault app


{-| The API answered `/txs/{identifier}/conversions` with the swap.
-}
gotSwapFor : String -> App -> App
gotSwapFor identifier =
    gotConversionsFor identifier [ dexSwap ]


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
requestedTxs =
    requestedTxsVerbatim >> List.map removeLeading0x


{-| The tx ids the last step asked the API for, exactly as they go on the wire.
-}
requestedTxsVerbatim : App -> List String
requestedTxsVerbatim =
    App.apiEffects >> List.filterMap requestedTxHash


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



-- MOVING THE SWAP ICON


{-| The one swap edge on the graph, once both legs are loaded.
-}
withSwapEdge : App -> App
withSwapEdge =
    withTx inputLegTx >> gotSwapFor inputLegId >> answerTxRequest outputLegTx


swapEdgeId : App -> Maybe ( Id, Id )
swapEdgeId app =
    (App.model app).network.conversions |> Dict.keys |> List.head


{-| The swap icon's stored offset, or an `Err` unless there is exactly one swap
edge, so no offset assertion passes on a graph without the edge.
-}
swapIconOffset : App -> Result String (Maybe { x : Float, y : Float })
swapIconOffset app =
    case Dict.values (App.model app).network.conversions of
        [ edge ] ->
            Ok edge.nodeOffset

        edges ->
            Err (String.fromInt (List.length edges) ++ " swap edges")


{-| Press on the swap icon at `from`, move the mouse to `to`, release.
-}
dragSwapIcon : { x : Float, y : Float } -> { x : Float, y : Float } -> App -> App
dragSwapIcon from to app =
    case swapEdgeId app of
        Just id ->
            app
                |> App.step (UserPushesLeftMouseButtonOnConversionNode id from)
                |> App.step (UserMovesMouseOnGraph to)
                |> App.step UserReleasesMouseButton

        Nothing ->
            app


{-| The swap edge is drawn only once both its addresses are on the graph.
-}
withSwapOnScreen : App -> App
withSwapOnScreen app =
    case swapEdgeId app of
        Just id ->
            App.mapModel
                (\m ->
                    { m
                        | network =
                            Network.updateConversionEdge id
                                (\edge ->
                                    { edge
                                        | inputAddress = Just (Address.init (Id.init "bnb" sender) { x = 0, y = 0 })
                                        , outputAddress = Just (Address.init (Id.init "bnb" swapper) { x = 0, y = 5 })
                                    }
                                )
                                m.network
                    }
                )
                app

        Nothing ->
            app


isSwapSelected : App -> Bool
isSwapSelected app =
    case ( (App.model app).selection, swapEdgeId app ) of
        ( SelectedConversionEdge selected, Just id ) ->
            selected == id

        _ ->
            False


dragVector : { x : Float, y : Float } -> { x : Float, y : Float } -> App -> { x : Float, y : Float }
dragVector from to app =
    Transform.vector from to (App.model app).transform


movingTheSwapIcon : Test
movingTheSwapIcon =
    describe "moving the swap icon"
        [ test "an edge starts where it always was" <|
            \_ ->
                App.init
                    |> withSwapEdge
                    |> swapIconOffset
                    |> Expect.equal (Ok Nothing)
        , test "dragging moves the icon by the mouse's way" <|
            \_ ->
                let
                    app =
                        App.init |> withSwapEdge
                in
                app
                    |> dragSwapIcon { x = 100, y = 100 } { x = 160, y = 130 }
                    |> swapIconOffset
                    |> Expect.equal (Ok (Just (dragVector { x = 100, y = 100 } { x = 160, y = 130 } app)))
        , test "after the release the mouse no longer moves it" <|
            \_ ->
                let
                    app =
                        App.init |> withSwapEdge
                in
                app
                    |> dragSwapIcon { x = 100, y = 100 } { x = 160, y = 130 }
                    |> App.step (UserMovesMouseOnGraph { x = 400, y = 400 })
                    |> swapIconOffset
                    |> Expect.equal (Ok (Just (dragVector { x = 100, y = 100 } { x = 160, y = 130 } app)))
        , test "a second drag continues from where the icon was left" <|
            \_ ->
                let
                    app =
                        App.init |> withSwapEdge

                    first =
                        dragVector { x = 100, y = 100 } { x = 160, y = 130 } app

                    second =
                        dragVector { x = 10, y = 10 } { x = 0, y = 50 } app
                in
                app
                    |> dragSwapIcon { x = 100, y = 100 } { x = 160, y = 130 }
                    |> dragSwapIcon { x = 10, y = 10 } { x = 0, y = 50 }
                    |> swapIconOffset
                    |> Expect.equal (Ok (Just { x = first.x + second.x, y = first.y + second.y }))
        , test "pressing and releasing the icon in place selects the swap" <|
            \_ ->
                App.init
                    |> withSwapEdge
                    |> dragSwapIcon { x = 100, y = 100 } { x = 100, y = 100 }
                    |> isSwapSelected
                    |> Expect.equal True
        , test "dragging the icon does not select the swap" <|
            \_ ->
                App.init
                    |> withSwapEdge
                    |> dragSwapIcon { x = 100, y = 100 } { x = 160, y = 130 }
                    |> (\app -> ( swapEdgeId app /= Nothing, isSwapSelected app ))
                    |> Expect.equal ( True, False )
        , test "the click that ends a drag stops at the icon" <|
            \_ ->
                App.init
                    |> withSwapEdge
                    |> withSwapOnScreen
                    |> App.html
                    |> Query.find [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-swap-node") ]
                    |> Event.simulate Event.click
                    |> Event.expect NoOp
        , test "the same swap answered again keeps the moved icon" <|
            \_ ->
                let
                    app =
                        App.init |> withSwapEdge
                in
                app
                    |> dragSwapIcon { x = 100, y = 100 } { x = 160, y = 130 }
                    |> gotSwapFor inputLegId
                    |> answerTxRequest outputLegTx
                    |> swapIconOffset
                    |> Expect.equal (Ok (Just (dragVector { x = 100, y = 100 } { x = 160, y = 130 } app)))
        ]



-- DRIVING THE WHOLE DASHBOARD


{-| The output leg names a synthesized input leg; the one leg request is
answered with a 404. `before`/`after` bracket that answer, `answered` names it.
-}
shellLegNotFound : { before : MainApp.App, after : MainApp.App, answered : List String }
shellLegNotFound =
    let
        withOutputLeg =
            MainApp.initAt "/"
                |> MainApp.step (Model.PathfinderMsg (BrowserGotTx (txRequest outputLegId) outputLegTx))

        asked =
            Dict.get (Id.init "bnb" outputLegId) (MainApp.model withOutputLeg).pathfinder.network.txs
                |> Maybe.map (\tx -> MainApp.step (Model.PathfinderMsg (BrowserGotConversions Set.empty tx [ nativeInSwap ])) withOutputLeg)
                |> Maybe.withDefault withOutputLeg

        ( tokened, tagged ) =
            Statusbar.messagesFromEffects (MainApp.model asked) (MainApp.effects asked)

        before =
            MainApp.mapModel (always tokened) asked

        ( after, answered ) =
            case List.filterMap apiRequest tagged of
                [ ( token, request ) ] ->
                    ( answer404 token request before, List.filterMap requestedTxHash [ request ] )

                _ ->
                    ( before, [] )
    in
    { before = before, after = after, answered = answered }


apiRequest : ( Maybe String, Model.Effect ) -> Maybe ( Maybe String, Effect.Api.Effect Model.Msg )
apiRequest ( token, eff ) =
    case eff of
        Model.ApiEffect request ->
            Just ( token, request )

        Model.PathfinderEffect (Effect.Pathfinder.ApiEffect request) ->
            Just ( token, Effect.Api.map Model.PathfinderMsg request )

        _ ->
            Nothing


requestedTxHash : Effect.Api.Effect msg -> Maybe String
requestedTxHash eff =
    case eff of
        Effect.Api.GetTxEffect { txHash } _ ->
            Just txHash

        Effect.Api.GetConversionLegEffect { txHash } _ ->
            Just txHash

        _ ->
            Nothing


answer404 : Maybe String -> Effect.Api.Effect Model.Msg -> MainApp.App -> MainApp.App
answer404 token request =
    MainApp.step
        (Model.BrowserGotResponseWithHeaders token
            (Err ( Http.BadStatus 404, Dict.empty, request ))
        )


{-| The open dialog's error type; `Just Nothing` for a dialog that is no error.
-}
dialogShown : MainApp.App -> Maybe (Maybe Dialog.ErrorType)
dialogShown app =
    (MainApp.model app).dialog
        |> Maybe.map
            (\dialog ->
                case dialog of
                    Dialog.Error { type_ } ->
                        Just type_

                    _ ->
                        Nothing
            )


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
                        |> App.step (BrowserGotTx (txRequest hash) inputLegTx)
                        |> requestedConversions
                        |> Expect.equal [ hash ]
            , test "a sub-transfer toggled on asks for its tx's swaps, by the tx hash" <|
                \_ ->
                    App.init
                        |> withTx inputLegTx
                        |> requestedConversions
                        |> Expect.equal [ hash ]
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
                \_ ->
                    let
                        asked =
                            App.init
                                |> withTx outputLegTx
                                |> gotSwapFor outputLegId
                    in
                    ( requestedTxs asked, answerTxRequest outputLegTx asked |> swapEdges )
                        |> Expect.equal ( [ inputLegId ], [] )
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
                -- pairing a renamed leg fails, and the handler would re-ask for it forever
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> gotSwapFor rootId
                        |> answerTxRequest (accountTx (hash ++ "_T42") swapper settlement)
                        |> (\app -> ( requestedTxs app, Dict.size (App.model app).network.txs ))
                        |> Expect.equal ( [], 1 )
            , test "the input-leg request answered with the output leg stops the walk" <|
                -- taking it would re-ask for the input leg and later pair the output leg with itself
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
                        |> answerTxRequest outputLegTx
                        |> (\app -> ( swapEdges app, Dict.size (App.model app).network.txs ))
                        |> Expect.equal ( [ ( inputLegId, outputLegId ) ], 3 )
            ]
        , describe "a leg already on the graph is not fetched again"
            [ test "the input leg pairs with an output leg in hand without a request" <|
                \_ ->
                    App.init
                        |> withTx outputLegTx
                        |> withTx inputLegTx
                        |> gotSwapFor inputLegId
                        |> (\app -> ( requestedTxs app, swapEdges app ))
                        |> Expect.equal ( [], [ ( inputLegId, outputLegId ) ] )
            , test "a whole-tx answer goes straight to the output leg when the input leg is in hand" <|
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> withTx inputLegTx
                        |> gotSwapFor rootId
                        |> requestedTxs
                        |> Expect.equal [ outputLegId ]
            , test "a whole-tx answer with both legs in hand pairs them without a request" <|
                \_ ->
                    App.init
                        |> withTx rootTx
                        |> withTx inputLegTx
                        |> withTx outputLegTx
                        |> gotSwapFor rootId
                        |> (\app -> ( requestedTxs app, swapEdges app, Dict.size (App.model app).network.txs ))
                        |> Expect.equal ( [], [ ( inputLegId, outputLegId ) ], 3 )
            ]
        , describe "a synthesized native leg is a normal, opaque leg"
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
        , describe "a synthesized leg the API answers with 404 ends the walk quietly"
            [ test "the one request answered is the synthesized leg's" <|
                \_ ->
                    shellLegNotFound
                        |> .answered
                        |> Expect.equal [ nativeInSwap.fromAssetTransfer ]
            , test "no dialog opens" <|
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
                                (BrowserGotTx (txRequest synthesizedLegId) >> Model.PathfinderMsg)

                        start =
                            MainApp.initAt "/"

                        ( tokened, tagged ) =
                            Statusbar.messagesFromEffects (MainApp.model start) [ Model.ApiEffect lookup ]
                    in
                    MainApp.mapModel (always tokened) start
                        |> answer404 (List.head tagged |> Maybe.andThen Tuple.first) lookup
                        |> dialogShown
                        |> Expect.equal (Just (Just (Dialog.TxNotFound [ synthesizedLegId ])))
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
                                , pegCurrency = Nothing
                                , ticker = "USD1"
                                }
                            ]
            , test "a leg without symbol and decimals registers nothing" <|
                \_ ->
                    App.init
                        |> withTx inputLegTx
                        |> gotSwapFor inputLegId
                        |> (\app -> ( requestedTxs app, App.outMsgs app ))
                        |> Expect.equal ( [ outputLegId ], [] )
            ]
        , movingTheSwapIcon
        ]
