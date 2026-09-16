module Scenario.AppTest exposing (suite)

{-| Whole-app scenarios: the parts of `Update.elm` that sit above the
Pathfinder — page routing, the statusbar, notifications, settings persistence
and theme switching. None of this was reachable from a test before
`Support.MainApp`.
-}

import Api.Data
import Autocomplete
import Dict
import Effect.Api
import Effect.Pathfinder
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Html.Attributes as Attributes
import Http
import Json.Decode
import Model exposing (Effect(..), Msg(..), Page(..))
import Model.Notification as Notification
import Model.Search
import Msg.Pathfinder
import Msg.Search
import RemoteData
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Update.Dialog
import Url exposing (Url)



-- HELPERS


page : App -> Page
page =
    App.model >> .page


url : String -> Url
url path =
    "https://example.com"
        ++ path
        |> Url.fromString
        |> Maybe.withDefault
            { protocol = Url.Https
            , host = "example.com"
            , port_ = Nothing
            , path = "/"
            , query = Nothing
            , fragment = Nothing
            }


withStats : (Api.Data.Stats -> Expectation) -> Expectation
withStats f =
    case Json.Decode.decodeString Api.Data.statsDecoder Fixture.stats of
        Ok stats ->
            f stats

        Err error ->
            Expect.fail ("the stats fixture did not decode: " ++ Json.Decode.errorToString error)


isSaveSettings : Effect -> Bool
isSaveSettings eff =
    case eff of
        SaveUserSettingsEffect _ ->
            True

        _ ->
            False


isNotification : Effect -> Bool
isNotification eff =
    case eff of
        NotificationEffect _ ->
            True

        _ ->
            False


isConsole : Effect -> Bool
isConsole eff =
    case eff of
        PortsConsoleEffect _ ->
            True

        _ ->
            False


{-| Any successful response, which is what moves the auth state to `Authorized`
— the state the user info is stored in.
-}
okResponse : Msg
okResponse =
    BrowserGotResponseWithHeaders Nothing (Ok ( Dict.empty, NoOp ))


{-| The settings page of a logged-in user the user endpoint reported `userInfo`
for.
-}
settingsPageWith : Effect.Api.UserInfo -> App
settingsPageWith userInfo =
    App.initAt "/settings"
        |> App.steps [ okResponse, BrowserGotUserInfo userInfo ]


hasUsernameRow : String -> App -> Expectation
hasUsernameRow username =
    App.html
        >> Query.find [ Selector.attribute (Attributes.attribute "data-testid" "gs-settings-username") ]
        >> Query.has [ Selector.text username ]


{-| A response that fails to decode, as a client/spec mismatch would produce.
-}
badBodyResponse : Msg
badBodyResponse =
    BrowserGotResponseWithHeaders Nothing
        (Err
            ( Http.BadBody "Problem with the given value"
            , Dict.empty
            , Effect.Api.GetStatisticsEffect BrowserGotStatistics
            )
        )



-- SCENARIOS


suite : Test
suite =
    describe "whole-app scenarios"
        [ describe "routing"
            [ test "the root url is the landing page" <|
                \_ -> App.initAt "/" |> page |> Expect.equal Home
            , test "/stats is the statistics page" <|
                \_ -> App.initAt "/stats" |> page |> Expect.equal Stats
            , test "/settings is the settings page" <|
                \_ -> App.initAt "/settings" |> page |> Expect.equal Settings
            , test "/pathfinder is the pathfinder" <|
                \_ -> App.initAt "/pathfinder" |> page |> Expect.equal Pathfinder
            , test "a legacy /graph url lands on the retired page" <|
                \_ ->
                    App.initAt "/graph/btc/address/1Archive1n2C579dMsAu3iC6tWzuQJz8dN"
                        |> page
                        |> Expect.equal RetiredGraph
            , test "navigating between pages ends up on the last one" <|
                \_ ->
                    App.initAt "/"
                        |> App.steps
                            [ BrowserChangedUrl (url "/stats")
                            , BrowserChangedUrl (url "/pathfinder")
                            , BrowserChangedUrl (url "/settings")
                            ]
                        |> page
                        |> Expect.equal Settings
            , test "navigating back to the pathfinder keeps the graph" <|
                \_ ->
                    -- leaving and returning must not reset the user's work
                    App.initAt "/pathfinder"
                        |> App.steps
                            [ BrowserChangedUrl (url "/stats")
                            , BrowserChangedUrl (url "/pathfinder")
                            ]
                        |> page
                        |> Expect.equal Pathfinder
            ]
        , describe "the rendered document"
            [ test "carries the product name in its title" <|
                \_ ->
                    App.initAt "/"
                        |> App.title
                        |> String.contains "Iknaio"
                        |> Expect.equal True
            , describe "every page renders"
                ([ "/", "/stats", "/settings", "/pathfinder", "/graph" ]
                    |> List.map
                        (\path ->
                            test path <|
                                \_ ->
                                    App.initAt path
                                        |> App.html
                                        |> Query.has [ Selector.tag "div" ]
                        )
                )
            ]
        , describe "statistics"
            -- The request itself is fired by Init.init, which the harness cannot
            -- call (see Support.MainApp); these cover what happens to the
            -- response, which is where the logic lives.
            [ test "are stored once the response arrives" <|
                \_ ->
                    withStats <|
                        \stats ->
                            App.initAt "/stats"
                                |> App.step (BrowserGotStatistics stats)
                                |> App.model
                                |> .stats
                                |> RemoteData.isSuccess
                                |> Expect.equal True
            , test "reach the supported-token config the value formatting needs" <|
                \_ ->
                    withStats <|
                        \stats ->
                            App.initAt "/stats"
                                |> App.step (BrowserGotStatistics stats)
                                |> App.model
                                |> .stats
                                |> RemoteData.map (.currencies >> List.length)
                                |> RemoteData.withDefault 0
                                |> Expect.greaterThan 0
            ]
        , describe "the user info on the settings page"
            -- the request is fired from handleResponse once the user is
            -- authorized; these cover what the page does with the response
            [ test "shows the username the user endpoint reported" <|
                \_ ->
                    settingsPageWith { expiration = Nothing, username = Just "alice" }
                        |> hasUsernameRow "alice"
            , test "shows no username row when the endpoint reported none" <|
                \_ ->
                    settingsPageWith { expiration = Nothing, username = Nothing }
                        |> App.html
                        |> Query.hasNot
                            [ Selector.attribute (Attributes.attribute "data-testid" "gs-settings-username") ]
            , test "labels the username row like the rows around it" <|
                \_ ->
                    -- the label is capitalized by View.Locale.string from the
                    -- case of the key, so a lowercase "username" key rendered
                    -- a lowercase label next to "Expires on"
                    settingsPageWith { expiration = Nothing, username = Just "alice" }
                        |> App.html
                        |> Query.find [ Selector.attribute (Attributes.attribute "data-testid" "gs-settings-username") ]
                        |> Query.has [ Selector.text "Username" ]
            , test "keeps the expiration row next to it" <|
                \_ ->
                    settingsPageWith { expiration = Nothing, username = Just "alice" }
                        |> App.html
                        -- untranslated in the test env, where a lookup falls
                        -- back to the key
                        |> Query.has [ Selector.text "expires on" ]
            , test "keeps the username across a later response" <|
                \_ ->
                    -- updateRequestLimit rebuilds the whole Authorized record on
                    -- every response; dropping the username there made it
                    -- disappear again a moment after it showed up
                    settingsPageWith { expiration = Nothing, username = Just "alice" }
                        |> App.step okResponse
                        |> hasUsernameRow "alice"
            ]
        , describe "user settings persist"
            [ test "switching the locale writes the settings" <|
                \_ ->
                    -- preferences go to localStorage through this effect; losing
                    -- it means they silently stop surviving a reload
                    App.initAt "/settings"
                        |> App.step (UserSwitchesLocale "de")
                        |> App.expectEffect "a SaveUserSettingsEffect" isSaveSettings
            , test "toggling light mode writes the settings" <|
                \_ ->
                    App.initAt "/settings"
                        |> App.step UserClickedLightmode
                        |> App.expectEffect "a SaveUserSettingsEffect" isSaveSettings
            , test "toggling light mode actually flips it" <|
                \_ ->
                    let
                        before =
                            App.initAt "/settings"
                    in
                    App.step UserClickedLightmode before
                        |> App.model
                        |> .config
                        |> .lightmode
                        |> Expect.notEqual (before |> App.model |> .config |> .lightmode)
            ]
        , describe "a response the client cannot decode"
            [ test "is logged in the statusbar rather than swallowed" <|
                \_ ->
                    App.initAt "/pathfinder"
                        |> App.step badBodyResponse
                        |> App.model
                        |> .statusbar
                        |> .log
                        |> List.length
                        |> Expect.greaterThan 0
            , test "raises a notification the user can actually see" <|
                \_ ->
                    App.initAt "/pathfinder"
                        |> App.step badBodyResponse
                        |> App.expectEffect "a NotificationEffect" isNotification
            , test "is echoed to the console for debugging" <|
                \_ ->
                    App.initAt "/pathfinder"
                        |> App.step badBodyResponse
                        |> App.expectEffect "a PortsConsoleEffect" isConsole
            , test "leaves the app usable" <|
                \_ ->
                    App.initAt "/pathfinder"
                        |> App.step badBodyResponse
                        |> App.steps [ BrowserChangedUrl (url "/settings") ]
                        |> page
                        |> Expect.equal Settings
            ]
        , describe "enter on one identifier the search matched nowhere"
            [ test "shows a transaction-not-found toast for a hash" <|
                \_ ->
                    App.initAt "/"
                        |> App.mapModel (\m -> { m | search = m.search |> typedAndAnsweredWithNothing xrpHash })
                        |> App.step (SearchMsg Msg.Search.UserClicksResultLine)
                        |> latestNotification
                        |> Expect.equal (Just { title = Just "transaction not found", message = "identifier-not-found", variables = [ xrpHash, "" ] })
            , test "and an address-not-found toast for anything else" <|
                \_ ->
                    App.initAt "/"
                        |> App.mapModel (\m -> { m | search = m.search |> typedAndAnsweredWithNothing xrpAddress })
                        |> App.step (SearchMsg Msg.Search.UserClicksResultLine)
                        |> latestNotification
                        |> Expect.equal (Just { title = Just "address not found", message = "identifier-not-found", variables = [ xrpAddress, "" ] })
            , test "names the searched networks once the statistics are in" <|
                \_ ->
                    withStats <|
                        \stats ->
                            App.initAt "/"
                                |> App.step (BrowserGotStatistics stats)
                                |> App.mapModel (\m -> { m | search = m.search |> typedAndAnsweredWithNothing xrpHash })
                                |> App.step (SearchMsg Msg.Search.UserClicksResultLine)
                                |> latestNotification
                                |> Expect.equal (Just { title = Just "transaction not found", message = "identifier-not-found-on-networks", variables = [ xrpHash, "BTC" ] })
            , test "opens no dialog, so the input can be corrected right away" <|
                \_ ->
                    App.initAt "/"
                        |> App.mapModel (\m -> { m | search = m.search |> typedAndAnsweredWithNothing xrpHash })
                        |> App.step (SearchMsg Msg.Search.UserClicksResultLine)
                        |> App.model
                        |> .dialog
                        |> Expect.equal Nothing
            , test "stays on the landing page instead of jumping to an empty Pathfinder" <|
                \_ ->
                    App.initAt "/"
                        |> App.mapModel (\m -> { m | search = m.search |> typedAndAnsweredWithNothing xrpHash })
                        |> App.step (SearchMsg Msg.Search.UserClicksResultLine)
                        |> App.expectNoEffect "navigation" isNavigation
            , test "does nothing while the search is still under way" <|
                \_ ->
                    App.initAt "/"
                        |> App.mapModel
                            (\m ->
                                { m
                                    | search =
                                        m.search
                                            |> Model.Search.setQuery xrpHash
                                            |> (\s -> { s | autocomplete = Autocomplete.setStatus Autocomplete.Fetching s.autocomplete })
                                }
                            )
                        |> App.step (SearchMsg Msg.Search.UserClicksResultLine)
                        |> latestNotification
                        |> Expect.equal Nothing
            , test "the Pathfinder's own search box shows the same toast" <|
                \_ ->
                    App.initAt "/pathfinder"
                        |> App.mapModel
                            (\m ->
                                let
                                    pathfinder =
                                        m.pathfinder
                                in
                                { m | pathfinder = { pathfinder | search = pathfinder.search |> typedAndAnsweredWithNothing xrpHash } }
                            )
                        |> App.step (PathfinderMsg (Msg.Pathfinder.SearchMsg Msg.Search.UserClicksResultLine))
                        |> latestNotification
                        |> Expect.equal (Just { title = Just "transaction not found", message = "identifier-not-found", variables = [ xrpHash, "" ] })
            ]
        , describe "the not-found dialog of a deep link"
            [ test "names the supported networks once the statistics are in" <|
                \_ ->
                    withStats <|
                        \stats ->
                            App.initAt "/"
                                |> App.step (BrowserGotStatistics stats)
                                |> App.mapModel (\m -> { m | dialog = Just (Update.Dialog.txNotFoundError xrpHash Nothing NoOp) })
                                |> App.html
                                |> Query.has [ Selector.text "Popup-not-found-unsupported-network" ]
            , test "and not before" <|
                \_ ->
                    App.initAt "/"
                        |> App.mapModel (\m -> { m | dialog = Just (Update.Dialog.txNotFoundError xrpHash Nothing NoOp) })
                        |> App.html
                        |> Query.hasNot [ Selector.text "Popup-not-found-unsupported-network" ]
            ]
        , describe "the search dropdown with no match"
            [ test "names the networks it searched once the statistics are in" <|
                \_ ->
                    withStats <|
                        \stats ->
                            App.initAt "/"
                                |> App.step (BrowserGotStatistics stats)
                                |> App.mapModel (\m -> { m | search = m.search |> typedAndAnsweredWithNothing xrpHash })
                                |> App.html
                                |> Query.has [ Selector.text "No-results-found-on-networks" ]
            , test "and just says so before" <|
                \_ ->
                    App.initAt "/"
                        |> App.mapModel (\m -> { m | search = m.search |> typedAndAnsweredWithNothing xrpHash })
                        |> App.html
                        |> Expect.all
                            [ Query.has [ Selector.text "No-results-found" ]
                            , Query.hasNot [ Selector.text "No-results-found-on-networks" ]
                            ]
            ]
        , describe "a search request that fails for good"
            [ test "settles the term in the pending multi-identifier paste" <|
                \_ ->
                    App.initAt "/pathfinder"
                        |> App.mapModel
                            (\m ->
                                let
                                    pathfinder =
                                        m.pathfinder
                                in
                                { m
                                    | pathfinder =
                                        { pathfinder
                                            | multiAdd =
                                                Just
                                                    { pending = [ "1FakeAddressThatDoesNotExist000000" ]
                                                    , total = 2
                                                    , added = 1
                                                    , notFound = []
                                                    , failed = []
                                                    , tooShort = []
                                                    }
                                        }
                                }
                            )
                        |> App.step (failedSearch "1FakeAddressThatDoesNotExist000000")
                        |> App.expectEffect "the paste's closing notification"
                            (\eff ->
                                case eff of
                                    PathfinderEffect (Effect.Pathfinder.ShowNotificationEffect _) ->
                                        True

                                    _ ->
                                        False
                            )
            ]
        ]


{-| An XRP transaction, which no supported network knows -- the case a user
reported as "the tool does not recognise the transaction".
-}
xrpHash : String
xrpHash =
    "D8FBA40D0E331BCE4EB2D9427635E76E12DC1D6DED26D77CDF8559BCDE07B715"


xrpAddress : String
xrpAddress =
    "rEb8TK3gBgk5auZkwc6sHnwrGVJH8DuaLh"


{-| A search box with `query` typed in and the search for it come back empty.
-}
typedAndAnsweredWithNothing : String -> Model.Search.Model -> Model.Search.Model
typedAndAnsweredWithNothing query search =
    search
        |> Model.Search.setQuery query
        |> (\s -> { s | visible = True, autocomplete = Autocomplete.setStatus Autocomplete.FetchedChoices s.autocomplete })


{-| Title, message key and variables of the newest toast.
-}
latestNotification : App -> Maybe { title : Maybe String, message : String, variables : List String }
latestNotification app =
    App.model app
        |> .notifications
        |> Notification.peek
        |> Maybe.map
            (\n ->
                case n of
                    Notification.Error d ->
                        { title = d.title, message = d.message, variables = d.variables }

                    Notification.Info d ->
                        { title = d.title, message = d.message, variables = d.variables }

                    Notification.Success d ->
                        { title = d.title, message = d.message, variables = d.variables }
            )


isNavigation : Effect -> Bool
isNavigation eff =
    case eff of
        NavPushUrlEffect _ ->
            True

        NavLoadEffect _ ->
            True

        _ ->
            False


{-| A search-box request for `query` that failed with no retry left.
-}
failedSearch : String -> Msg
failedSearch query =
    BrowserGotResponseWithHeaders Nothing
        (Err
            ( Http.NetworkError
            , Dict.empty
            , Effect.Api.SearchEffect
                { query = query
                , currency = Nothing
                , limit = Just 1
                , config = Effect.Api.defaultSearchConfig
                }
                (always NoOp)
            )
        )
