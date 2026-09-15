module Scenario.PathfinderTest exposing (suite)

{-| End-to-end scenarios for the Pathfinder, driven through the real update and
view by `Support.App` — the same ground a browser test would cover, without a
browser.

The API responses are the ones the OpenAPI spec documents (`Fixtures.Api`), so
these scenarios break if either the app logic or the API contract moves.

-}

import Api.Data
import Autocomplete
import Config.Pathfinder exposing (TracingMode(..))
import Dict
import Effect.Api
import Effect.Pathfinder
import Effect.Search
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Json.Decode
import List.Extra
import Model.Direction exposing (Direction(..))
import Model.Notification as Notification
import Model.Pathfinder exposing (Details(..))
import Model.Pathfinder.Address exposing (Txs(..))
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Selection exposing (Selection(..))
import Model.Search as Search
import Msg.Pathfinder exposing (Msg(..), OutMsg(..))
import Msg.Search as Search
import Route.Pathfinder as Route
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Update.Pathfinder
import Update.Search



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
        , describe "enter on one identifier the search matched nowhere"
            [ test "asks the shell for the not-found dialog" <|
                \_ ->
                    App.init
                        |> App.mapModel (\m -> { m | search = searchModelWith unknownTerm |> answeredWithNothing })
                        |> App.step (SearchMsg Search.UserClicksResultLine)
                        |> App.outMsgs
                        |> Expect.equalLists [ IdentifierNotFound unknownTerm ]
            , test "not while the search is still under way" <|
                \_ ->
                    App.init
                        |> App.mapModel (\m -> { m | search = searchModelWith unknownTerm |> stillSearching })
                        |> App.step (SearchMsg Search.UserClicksResultLine)
                        |> App.outMsgs
                        |> Expect.equalLists []
            , test "and not for a paste of several identifiers, which is added term by term" <|
                \_ ->
                    App.init
                        |> App.mapModel (\m -> { m | search = searchModelWith (knownTerm ++ ", " ++ unknownTerm) |> answeredWithNothing })
                        |> App.step (SearchMsg Search.UserClicksResultLine)
                        |> App.outMsgs
                        |> Expect.equalLists []
            ]
        , describe "pasting several identifiers into the search box"
            [ test "is not searched as one string by the autocomplete" <|
                \_ ->
                    searchModelWith (String.join ", " [ knownTerm, "abc123", unknownTerm ])
                        |> Update.Search.maybeTriggerSearch
                        |> List.filterMap searchQuery
                        |> Expect.equalLists []
            , test "while a single identifier still is" <|
                \_ ->
                    searchModelWith knownTerm
                        |> Update.Search.maybeTriggerSearch
                        |> List.filterMap searchQuery
                        |> Expect.equalLists [ knownTerm ]
            , test "keeps the multi-term hint over an empty autocomplete result" <|
                \_ ->
                    App.init
                        |> App.mapModel
                            (\m ->
                                { m
                                    | search =
                                        searchModelWith (String.join ", " [ knownTerm, "abc123", unknownTerm ])
                                            |> answeredWithNothing
                                }
                            )
                        |> App.html
                        |> Expect.all
                            [ Query.has [ Selector.text "Hint-multiple-search-terms" ]
                            , Query.hasNot [ Selector.text "No-results-found" ]
                            ]
            , test "sends one search request per term" <|
                \_ ->
                    pasted [ knownTerm, unknownTerm ]
                        |> App.searchEffects
                        |> List.filterMap searchQuery
                        |> Expect.equalLists [ knownTerm, unknownTerm ]
            , test "adds the matches to the graph" <|
                \_ ->
                    withSearchFixture <|
                        \found ->
                            pasted [ knownTerm, unknownTerm ]
                                |> App.respondSearch (answers [ ( knownTerm, found ) ])
                                |> App.apiEffects
                                |> List.filterMap requestedAddress
                                |> Expect.equal [ addressId, ( "btc", otherTerm ) ]
            , test "says nothing while a term is still pending" <|
                \_ ->
                    pasted [ knownTerm, unknownTerm ]
                        |> App.respondSearch (answers [ ( unknownTerm, noMatch ) ])
                        |> App.expectNoEffect "notification" isNotification
            , test "reports a term nothing matched once every term has answered" <|
                \_ ->
                    withSearchFixture <|
                        \found ->
                            pasted [ knownTerm, unknownTerm ]
                                |> App.respondSearch (answers [ ( unknownTerm, noMatch ), ( knownTerm, found ) ])
                                |> App.expectEffect "info listing the unmatched term"
                                    (isSkippedNotification
                                        { level = "info"
                                        , skipped = 1
                                        , total = 2
                                        , reasons = [ "multi-add-skipped-not-found" ]
                                        , notFound = unknownTerm
                                        , failed = ""
                                        , tooShort = ""
                                        }
                                    )
            , test "reports a term whose request failed for good" <|
                \_ ->
                    withSearchFixture <|
                        \found ->
                            pasted [ knownTerm, unknownTerm ]
                                |> App.respondSearch (answers [ ( knownTerm, found ) ])
                                |> App.step (SearchMsg (Search.BrowserGotMultiSearchError unknownTerm))
                                |> App.expectEffect "info listing the failed term"
                                    (isSkippedNotification
                                        { level = "info"
                                        , skipped = 1
                                        , total = 2
                                        , reasons = [ "multi-add-skipped-failed" ]
                                        , notFound = ""
                                        , failed = unknownTerm
                                        , tooShort = ""
                                        }
                                    )
            , test "reports a token too short to be searched for" <|
                \_ ->
                    withSearchFixture <|
                        \found ->
                            pasted [ knownTerm, "abc123", otherTerm ]
                                |> App.respondSearch (answers [ ( knownTerm, found ), ( otherTerm, found ) ])
                                |> App.expectEffect "info listing the short token"
                                    (isSkippedNotification
                                        { level = "info"
                                        , skipped = 1
                                        , total = 3
                                        , reasons = [ "multi-add-skipped-too-short" ]
                                        , notFound = ""
                                        , failed = ""
                                        , tooShort = "abc123"
                                        }
                                    )
            , test "is an error when nothing at all could be added" <|
                \_ ->
                    pasted [ unknownTerm, otherTerm ]
                        |> App.respondSearch (answers [ ( unknownTerm, noMatch ), ( otherTerm, noMatch ) ])
                        |> App.expectEffect "error listing both terms"
                            (isSkippedNotification
                                { level = "error"
                                , skipped = 2
                                , total = 2
                                , reasons = [ "multi-add-skipped-not-found" ]
                                , notFound = unknownTerm ++ ", " ++ otherTerm
                                , failed = ""
                                , tooShort = ""
                                }
                            )
            , test "stays quiet when every term was added" <|
                \_ ->
                    withSearchFixture <|
                        \found ->
                            pasted [ knownTerm, otherTerm ]
                                |> App.respondSearch (answers [ ( knownTerm, found ), ( otherTerm, found ) ])
                                |> App.expectNoEffect "notification" isNotification
            , test "ignores an answer to a paste that was replaced" <|
                \_ ->
                    withSearchFixture <|
                        \found ->
                            pasted [ knownTerm, unknownTerm ]
                                |> pastedAgain [ otherTerm, knownTerm ]
                                |> App.respondSearch (answers [ ( otherTerm, found ) ])
                                |> App.step (SearchMsg (Search.BrowserGotMultiSearchResult unknownTerm noMatch))
                                |> App.expectNoEffect "notification" isNotification
            ]
        ]



-- PASTING SEVERAL IDENTIFIERS


knownTerm : String
knownTerm =
    Tuple.second addressId


{-| The second address of the search fixture.
-}
otherTerm : String
otherTerm =
    "1ArchiveisY6i4Hpostivemate1sVRhQ71"


unknownTerm : String
unknownTerm =
    "1FakeAddressThatDoesNotExist000000"


{-| What the search endpoint answers when nothing matches.
-}
noMatch : Api.Data.SearchResult
noMatch =
    { actors = Nothing, currencies = [], labels = [] }


withSearchFixture : (Api.Data.SearchResult -> Expectation) -> Expectation
withSearchFixture f =
    case Json.Decode.decodeString Api.Data.searchResultDecoder Fixture.searchResult of
        Ok result ->
            f result

        Err error ->
            Expect.fail ("the search result fixture did not decode: " ++ Json.Decode.errorToString error)


{-| The Pathfinder's search box with `query` typed into it.
-}
searchModelWith : String -> Search.Model
searchModelWith query =
    App.init |> App.model |> .search |> Search.setQuery query


{-| ... whose search has come back empty.
-}
answeredWithNothing : Search.Model -> Search.Model
answeredWithNothing s =
    { s | visible = True, autocomplete = Autocomplete.setStatus Autocomplete.FetchedChoices s.autocomplete }


{-| ... whose search has not answered yet.
-}
stillSearching : Search.Model -> Search.Model
stillSearching s =
    { s | visible = True, autocomplete = Autocomplete.setStatus Autocomplete.Fetching s.autocomplete }


{-| Pastes the terms, comma-separated, into the search box and hits enter.
-}
pasted : List String -> App
pasted terms =
    pastedAgain terms App.init


pastedAgain : List String -> App -> App
pastedAgain terms =
    App.mapModel (\m -> { m | search = Search.setQuery (String.join ", " terms) m.search })
        >> App.step (SearchMsg Search.UserClicksResultLine)


{-| Answers the search requests for the listed terms, leaving the others
pending. All answers go in one call because the harness only keeps the effects
of the latest step.
-}
answers : List ( String, Api.Data.SearchResult ) -> Effect.Search.Effect -> Maybe Search.Msg
answers results eff =
    case eff of
        Effect.Search.SearchEffect { query, toMsg } ->
            results
                |> List.Extra.find (Tuple.first >> (==) query)
                |> Maybe.map (Tuple.second >> toMsg)

        _ ->
            Nothing


searchQuery : Effect.Search.Effect -> Maybe String
searchQuery eff =
    case eff of
        Effect.Search.SearchEffect { query } ->
            Just query

        _ ->
            Nothing


requestedAddress : Effect.Api.Effect Msg -> Maybe Id
requestedAddress eff =
    case eff of
        Effect.Api.GetAddressEffect { currency, address } _ ->
            Just ( currency, address )

        _ ->
            Nothing


isNotification : Effect.Pathfinder.Effect -> Bool
isNotification eff =
    case eff of
        Effect.Pathfinder.ShowNotificationEffect _ ->
            True

        _ ->
            False


{-| The one notification a paste with skipped tokens ends in: its level, the
reason lines it shows, and the variables those lines and the "{0} of {1}"
message are rendered with.
-}
isSkippedNotification :
    { level : String, skipped : Int, total : Int, reasons : List String, notFound : String, failed : String, tooShort : String }
    -> Effect.Pathfinder.Effect
    -> Bool
isSkippedNotification expected eff =
    let
        matches level data =
            level
                == expected.level
                && data.message
                == "multi-add-skipped-message"
                && data.moreInfo
                == expected.reasons
                && data.variables
                == [ String.fromInt expected.skipped
                   , String.fromInt expected.total
                   , expected.notFound
                   , expected.failed
                   , expected.tooShort
                   ]
    in
    case eff of
        Effect.Pathfinder.ShowNotificationEffect (Notification.Info data) ->
            matches "info" data

        Effect.Pathfinder.ShowNotificationEffect (Notification.Error data) ->
            matches "error" data

        _ ->
            False


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
