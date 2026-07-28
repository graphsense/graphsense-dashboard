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
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Route.Pathfinder as Route
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector



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
        , describe "the empty graph"
            [ test "starts with nothing on it" <|
                \_ ->
                    App.init |> addressesOnGraph |> Expect.equal []
            , test "still renders" <|
                \_ ->
                    App.init |> App.html |> Query.has [ Selector.tag "svg" ]
            ]
        ]
