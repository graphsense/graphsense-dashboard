module Scenario.AppTest exposing (suite)

{-| Whole-app scenarios: the parts of `Update.elm` that sit above the
Pathfinder — page routing, the statusbar, notifications, settings persistence
and theme switching. None of this was reachable from a test before
`Support.MainApp`.
-}

import Api.Data
import Dict
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Http
import Json.Decode
import Model exposing (Effect(..), Msg(..), Page(..))
import RemoteData
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
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
        ]
