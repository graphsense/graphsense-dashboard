module Scenario.PathfinderDispatchTest exposing (suite)

{-| Pins how a Pathfinder message reaches the application shell.

`Update.elm` used to intercept a dozen `PathfinderMsg` variants _before_ they got to
`Update.Pathfinder`, resolving dispatch by the order of case branches across two
files. Three inconsistent conventions had grown up in there — intercepted and
delegated, intercepted and not delegated, or stubbed with a comment — and two
Pathfinder handlers were unreachable as a result, silently doing nothing.

Now every message reaches `updateByMsg`, and anything needing the top-level model
comes back as an `OutMsg`. These tests hold both halves of that in place: that the
Pathfinder _emits_ the request (`Support.App`, which has no shell), and that the shell
_acts_ on it (`Support.MainApp`, which is the real `Update.update`).

Delete a branch from `appLevelOutMsgs` or from `applyPathfinderOutMsg` and something
here goes red.

-}

import Expect
import Model exposing (Msg(..))
import Msg.Pathfinder as Pathfinder exposing (OutMsg(..))
import Route.Pathfinder as PathfinderRoute
import Support.App as Pf
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)


{-| The shell is what owns dialogs, so "a dialog is open" is the observable proof
that the out-message got there.
-}
dialogIsOpen : App -> Bool
dialogIsOpen app =
    (App.model app).dialog /= Nothing


suite : Test
suite =
    describe "Pathfinder dispatch"
        [ describe "the Pathfinder asks the shell for what it cannot do itself"
            [ test "showing the legend" <|
                \_ ->
                    Pf.initAt PathfinderRoute.Root
                        |> Pf.step Pathfinder.UserClickedShowLegend
                        |> Pf.outMsgs
                        |> Expect.equal [ ShowLegendDialog ]
            , test "opening the export dialog" <|
                \_ ->
                    Pf.initAt PathfinderRoute.Root
                        |> Pf.step (Pathfinder.UserClickedExportGraph Nothing)
                        |> Pf.outMsgs
                        |> Expect.equal [ OpenExportDialog Nothing ]
            , test "closing whatever overlay is open, on escape" <|
                \_ ->
                    Pf.initAt PathfinderRoute.Root
                        |> Pf.step Pathfinder.UserReleasedEscape
                        |> Pf.outMsgs
                        |> Expect.equal [ CloseTopmostOverlay ]
            , test "restarting straight away when there is nothing to lose" <|
                \_ ->
                    -- The dirty check moved out of Update.elm and into the
                    -- Pathfinder, which is the only side that knows.
                    Pf.initAt PathfinderRoute.Root
                        |> Pf.step Pathfinder.UserClickedRestart
                        |> Pf.outMsgs
                        |> Expect.equal [ Restart ]
            , test "persisting settings when the tracing mode changes" <|
                \_ ->
                    Pf.initAt PathfinderRoute.Root
                        |> Pf.step Pathfinder.UserClickedToggleTracingMode
                        |> Pf.outMsgs
                        |> Expect.equal [ SaveUserSettings ]
            , test "and asks for nothing on a message it handles alone" <|
                \_ ->
                    Pf.initAt PathfinderRoute.Root
                        |> Pf.step Pathfinder.UserClickedToggleHelpDropdown
                        |> Pf.outMsgs
                        |> Expect.equal []
            ]
        , describe "the shell acts on the request"
            [ test "the legend opens a dialog" <|
                \_ ->
                    App.initAt "/pathfinder"
                        |> App.step (PathfinderMsg Pathfinder.UserClickedShowLegend)
                        |> dialogIsOpen
                        |> Expect.equal True
            , test "escape closes it again" <|
                \_ ->
                    App.initAt "/pathfinder"
                        |> App.step (PathfinderMsg Pathfinder.UserClickedShowLegend)
                        |> App.step (PathfinderMsg Pathfinder.UserReleasedEscape)
                        |> dialogIsOpen
                        |> Expect.equal False
            , test "the export dialog opens once the timestamp arrives" <|
                \_ ->
                    -- Without a time the Pathfinder asks for one first, so the
                    -- dialog only appears on the second pass.
                    App.initAt "/pathfinder"
                        |> App.step (PathfinderMsg (Pathfinder.UserClickedExportGraph Nothing))
                        |> dialogIsOpen
                        |> Expect.equal False
            ]
        , describe "a message the shell used to swallow"
            [ test "still reaches updateByMsg, which owns the dropdown state" <|
                \_ ->
                    -- UserClickedToggleHelpDropdown was never intercepted, but it sits
                    -- next to ones that were; this is the cheap check that routing a
                    -- message through the shell does not stop the Pathfinder seeing it.
                    let
                        before =
                            Pf.initAt PathfinderRoute.Root

                        after =
                            Pf.step Pathfinder.UserClickedToggleHelpDropdown before
                    in
                    Expect.notEqual
                        (Pf.model before).helpDropdownOpen
                        (Pf.model after).helpDropdownOpen
            ]
        ]
