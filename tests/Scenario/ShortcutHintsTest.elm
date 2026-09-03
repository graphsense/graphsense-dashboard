module Scenario.ShortcutHintsTest exposing (suite)

{-| The shortcut hint overlay shows once Ctrl/Cmd has been held for a moment and
goes away with the key. The delay is a timer (`RuntimeModKeyHeld`) tied to the
press that started it, so a quick chord or a Ctrl+click never sees the overlay
and a stale timer cannot show it for a later press.
-}

import Effect.Pathfinder exposing (Effect(..))
import Expect
import Html.Attributes
import Msg.Pathfinder exposing (Msg(..))
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Util.Pathfinder.Shortcuts as Shortcuts


hintsShown : App -> Bool
hintsShown =
    App.model >> .showShortcutHints


overlay : List Selector
overlay =
    [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-shortcut-hints") ]


isCmd : Effect -> Bool
isCmd eff =
    case eff of
        CmdEffect _ ->
            True

        _ ->
            False


suite : Test
suite =
    describe "shortcut hints"
        [ test "pressing the mod key starts the timer" <|
            \_ ->
                App.init
                    |> App.step UserPressedModKey
                    |> App.expectEffect "a delayed RuntimeModKeyHeld" isCmd
        , test "the overlay is not shown before the timer fires" <|
            \_ ->
                App.init
                    |> App.step UserPressedModKey
                    |> App.html
                    |> Query.hasNot overlay
        , test "the overlay shows when the timer fires while the key is still held" <|
            \_ ->
                App.init
                    |> App.steps [ UserPressedModKey, RuntimeModKeyHeld 1 ]
                    |> App.html
                    |> Query.has (Selector.text "Keyboard shortcuts" :: overlay)
        , test "the overlay lists the add-address and find-on-graph chords" <|
            \_ ->
                App.init
                    |> App.steps [ UserPressedModKey, RuntimeModKeyHeld 1 ]
                    |> App.html
                    |> Query.find overlay
                    |> Query.has [ Selector.text "Add address or tx", Selector.text "Find on graph" ]
        , test "Ctrl+K asks the browser to focus the address search box" <|
            \_ ->
                -- Dom.focus is a Cmd, so all the harness can see is that one was issued
                App.init
                    |> App.step (UserPressedHotkey "k")
                    |> App.expectEffect "a focus command" isCmd
        , test "releasing the key hides the overlay" <|
            \_ ->
                App.init
                    |> App.steps [ UserPressedModKey, RuntimeModKeyHeld 1, UserReleasedModKey ]
                    |> hintsShown
                    |> Expect.equal False
        , test "a timer that fires after the key was released shows nothing" <|
            \_ ->
                App.init
                    |> App.steps [ UserPressedModKey, UserReleasedModKey, RuntimeModKeyHeld 1 ]
                    |> hintsShown
                    |> Expect.equal False
        , test "a timer from an earlier press does not show hints for a later one" <|
            \_ ->
                -- press, release, press again inside the delay: the first timer
                -- fires with count 1 while the current press is count 2
                App.init
                    |> App.steps [ UserPressedModKey, UserReleasedModKey, UserPressedModKey, RuntimeModKeyHeld 1 ]
                    |> hintsShown
                    |> Expect.equal False
        , test "the timer of the current press does show them" <|
            \_ ->
                App.init
                    |> App.steps [ UserPressedModKey, UserReleasedModKey, UserPressedModKey, RuntimeModKeyHeld 2 ]
                    |> hintsShown
                    |> Expect.equal True
        , test "keydown auto-repeat neither restarts the timer nor counts as a new press" <|
            \_ ->
                App.init
                    |> App.steps [ UserPressedModKey, UserPressedModKey ]
                    |> App.model
                    |> .modKeyPressCount
                    |> Expect.equal 1
        , describe "toolbar tooltips"
            [ test "carry the chord" <|
                \_ ->
                    App.init
                        |> App.html
                        |> Query.find [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-toolbar-save") ]
                        |> Query.has [ Selector.attribute (Html.Attributes.title "save file (Ctrl+S)") ]
            , test "read Cmd on macOS" <|
                \_ ->
                    Shortcuts.chord True Shortcuts.save
                        |> Expect.equal "⌘S"
            , test "read Ctrl elsewhere" <|
                \_ ->
                    Shortcuts.chord False Shortcuts.undo
                        |> Expect.equal "Ctrl+Z"
            , test "plain keys have no modifier" <|
                \_ ->
                    Shortcuts.chord True Shortcuts.deleteSelection
                        |> Expect.equal "Del"
            ]
        ]
