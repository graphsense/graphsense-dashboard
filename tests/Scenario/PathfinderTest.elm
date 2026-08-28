module Scenario.PathfinderTest exposing (suite)

{-| End-to-end scenarios for the Pathfinder, driven through the real update and
view by `Support.App` — the same ground a browser test would cover, without a
browser.

The API responses are the ones the OpenAPI spec documents (`Fixtures.Api`), so
these scenarios break if either the app logic or the API contract moves.

-}

import Api.Data
import Config.Pathfinder exposing (TracingMode(..))
import Dict
import Effect.Api
import Effect.Pathfinder
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Json.Decode
import Model.Direction exposing (Direction(..))
import Model.Pathfinder exposing (Details(..))
import Model.Pathfinder.Address exposing (Txs(..))
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Selection exposing (Selection(..))
import Model.Search as Search
import Msg.Pathfinder exposing (Msg(..))
import Route.Pathfinder as Route
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Update.Pathfinder



-- FIXTURES


{-| The address the spec's own example describes.
-}
addressId : Id
addressId =
    ( "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" )


{-| Runs `f` with the decoded fixture, or fails with a clear message. Keeps the
scenarios free of `Result` plumbing.
-}
withAddressFixture : (Api.Data.Address -> Expectation) -> Expectation
withAddressFixture f =
    case Json.Decode.decodeString Api.Data.addressDecoder Fixture.address of
        Ok address ->
            f address

        Err error ->
            Expect.fail ("the address fixture did not decode: " ++ Json.Decode.errorToString error)


{-| Answers a pending address request with the fixture.
-}
answerAddress : Api.Data.Address -> App -> App
answerAddress address =
    App.respond
        (\eff ->
            case eff of
                Effect.Api.GetAddressEffect _ toMsg ->
                    Just (toMsg address)

                _ ->
                    Nothing
        )


isAddressRequest : Effect.Pathfinder.Effect -> Bool
isAddressRequest eff =
    case eff of
        Effect.Pathfinder.ApiEffect (Effect.Api.GetAddressEffect _ _) ->
            True

        Effect.Pathfinder.BatchEffect batched ->
            List.any isAddressRequest batched

        _ ->
            False


addressesOnGraph : App -> List Id
addressesOnGraph =
    App.model >> .network >> .addresses >> Dict.keys


{-| Opens the deep link and answers the address request it fires.
-}
graphWithOneAddress : Api.Data.Address -> App
graphWithOneAddress address =
    Route.addressRoute { network = "btc", address = Tuple.second addressId }
        |> App.initAt
        |> answerAddress address



-- SCENARIOS


suite : Test
suite =
    describe "Pathfinder scenarios"
        [ describe "opening an address deep link"
            [ test "asks the API for the address" <|
                \_ ->
                    Route.addressRoute { network = "btc", address = Tuple.second addressId }
                        |> App.initAt
                        |> App.expectEffect "a GetAddressEffect" isAddressRequest
            , test "puts the address on the graph once the response arrives" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> addressesOnGraph
                                |> Expect.equal [ addressId ]
            , test "renders the address label on the node" <|
                \_ ->
                    -- graph nodes shorten the identifier; the full id only ever
                    -- appears in attributes, never as visible text
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.html
                                |> Query.has [ Selector.text "1Archive…z8dN" ]
            ]
        , describe "removing an address"
            [ test "takes it off the graph" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.step (UserClickedRemoveAddressFromGraph addressId)
                                |> addressesOnGraph
                                |> Expect.equal []
            ]
        , describe "undo and redo"
            [ test "undo brings a removed address back" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.step (UserClickedRemoveAddressFromGraph addressId)
                                |> App.step UserClickedUndo
                                |> addressesOnGraph
                                |> Expect.equal [ addressId ]
            , test "redo removes it again" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.steps
                                    [ UserClickedRemoveAddressFromGraph addressId
                                    , UserClickedUndo
                                    , UserClickedRedo
                                    ]
                                |> addressesOnGraph
                                |> Expect.equal []
            ]
        , describe "the tracing mode toggle"
            -- The two branches of `relations` in View.Pathfinder.Network render a
            -- different number of keyed children at the same position, which is
            -- what used to crash the virtual DOM. Rendering both keeps that path
            -- exercised.
            [ test "flips the mode" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.step UserClickedToggleTracingMode
                                |> App.model
                                |> .config
                                |> .tracingMode
                                |> Expect.notEqual TransactionTracingMode
            , test "renders in both modes" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.step UserClickedToggleTracingMode
                                |> App.html
                                |> Query.has [ Selector.tag "svg" ]
            ]
        , describe "picking a search result"
            -- The search box hands back a ResultLine; the route it maps to is
            -- what actually puts something on the graph. Getting this mapping
            -- wrong sends the user to the wrong page with no error.
            [ test "an address result routes to that address" <|
                \_ ->
                    Update.Pathfinder.resultLineToRoute (Search.Address "btc" (Tuple.second addressId))
                        |> Expect.equal (Route.Network "btc" (Route.Address (Tuple.second addressId) Nothing))
            , test "a transaction result routes to that transaction" <|
                \_ ->
                    Update.Pathfinder.resultLineToRoute (Search.Tx "btc" "abc123")
                        |> Expect.equal (Route.Network "btc" (Route.Tx "abc123"))
            , test "a block result routes to that block" <|
                \_ ->
                    Update.Pathfinder.resultLineToRoute (Search.Block "btc" 42)
                        |> Expect.equal (Route.Network "btc" (Route.Block 42))
            , test "a label result routes to the label page" <|
                \_ ->
                    Update.Pathfinder.resultLineToRoute (Search.Label "internet archive")
                        |> Expect.equal (Route.Label "internet archive")
            , test "an actor result routes to the actor page" <|
                \_ ->
                    Update.Pathfinder.resultLineToRoute (Search.Actor ( "binance", "Binance" ))
                        |> Expect.equal (Route.Actor "binance")
            , test "the route it produces fetches the address" <|
                \_ ->
                    -- the whole point of the mapping: search, pick, node appears
                    withAddressFixture <|
                        \address ->
                            Update.Pathfinder.resultLineToRoute
                                (Search.Address "btc" (Tuple.second addressId))
                                |> App.initAt
                                |> answerAddress address
                                |> addressesOnGraph
                                |> Expect.equal [ addressId ]
            ]
        , describe "selecting a node"
            [ test "marks it as the selection" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.step (UserClickedAddress addressId)
                                |> App.model
                                |> .selection
                                |> Expect.equal (SelectedAddress addressId)
            , test "opens its details panel" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.step (UserClickedAddress addressId)
                                |> App.model
                                |> .details
                                |> Maybe.map isAddressDetails
                                |> Expect.equal (Just True)
            , test "closing the details view clears the selection" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.steps [ UserClickedAddress addressId, UserClosedDetailsView ]
                                |> App.model
                                |> (\m -> ( m.selection, m.details ))
                                |> Expect.equal ( NoSelection, Nothing )
            , test "deleting the selected node takes the panel with it" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.steps
                                    [ UserClickedAddress addressId
                                    , UserClickedRemoveAddressFromGraph addressId
                                    ]
                                |> App.model
                                |> .details
                                |> Expect.equal Nothing
            ]
        , describe "expanding an address"
            [ test "selects it" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.step (UserClickedAddressExpandHandle addressId Outgoing)
                                |> App.model
                                |> .selection
                                |> Expect.equal (SelectedAddress addressId)
            , test "asks the API for what to expand into" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.step (UserClickedAddressExpandHandle addressId Outgoing)
                                |> App.expectEffect "an API request" isAnyApiRequest
            , test "marks the direction as loading, so the handle cannot be double-fired" <|
                \_ ->
                    withAddressFixture <|
                        \address ->
                            graphWithOneAddress address
                                |> App.step (UserClickedAddressExpandHandle addressId Outgoing)
                                |> App.model
                                |> .network
                                |> .addresses
                                |> Dict.get addressId
                                |> Maybe.map (.outgoingTxs >> isLoading)
                                |> Expect.equal (Just True)
            , test "a second click while loading asks for nothing more" <|
                \_ ->
                    -- asserting the second click is silent is only meaningful
                    -- next to the first one being loud, so compare the two
                    withAddressFixture <|
                        \address ->
                            let
                                afterFirst =
                                    graphWithOneAddress address
                                        |> App.step (UserClickedAddressExpandHandle addressId Outgoing)

                                afterSecond =
                                    afterFirst
                                        |> App.step (UserClickedAddressExpandHandle addressId Outgoing)
                            in
                            ( List.isEmpty (App.apiEffects afterFirst)
                            , List.isEmpty (App.apiEffects afterSecond)
                            )
                                |> Expect.equal ( False, True )
            ]
        , describe "the empty graph"
            [ test "starts with nothing on it" <|
                \_ ->
                    App.init |> addressesOnGraph |> Expect.equal []
            , test "still renders" <|
                \_ ->
                    App.init |> App.html |> Query.has [ Selector.tag "svg" ]
            ]
        ]


isAddressDetails : Details -> Bool
isAddressDetails details =
    case details of
        AddressDetails _ _ ->
            True

        _ ->
            False


isLoading : Txs -> Bool
isLoading txs =
    case txs of
        TxsLoading ->
            True

        _ ->
            False


isAnyApiRequest : Effect.Pathfinder.Effect -> Bool
isAnyApiRequest eff =
    case eff of
        Effect.Pathfinder.ApiEffect _ ->
            True

        Effect.Pathfinder.BatchEffect batched ->
            List.any isAnyApiRequest batched

        _ ->
            False
