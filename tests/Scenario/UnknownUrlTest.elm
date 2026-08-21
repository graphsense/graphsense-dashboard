module Scenario.UnknownUrlTest exposing (suite)

{-| An unrecognised URL must not put the runtime in a retry loop.

`Update.updateByUrl` reschedules itself when `Route.parse` returns `Nothing`, because at
boot a Pathfinder deep link cannot be parsed until `model.stats` arrives and supplies the
network names. Nothing used to bound that retry, so a URL that will _never_ parse -- a
typo, or a plugin namespace that has since been renamed -- rescheduled itself every 50ms
forever, with no user action able to escape it.

The two halves both matter, so both are pinned here: still retry while the statistics are
in flight, stop once they have settled.

-}

import Expect
import Http
import Model exposing (Effect(..))
import RemoteData
import Support.MainApp as App
import Test exposing (Test, describe, test)


isPostpone : Effect -> Bool
isPostpone effect =
    case effect of
        PostponeUpdateByUrlEffect _ ->
            True

        _ ->
            False


isNotification : Effect -> Bool
isNotification effect =
    case effect of
        NotificationEffect _ ->
            True

        _ ->
            False


{-| Any settled state will do; a failed statistics request is the cheapest to write and
is itself a real case -- the request failing must not turn a bad URL into a busy loop.
-}
settled : RemoteData.WebData never_
settled =
    RemoteData.Failure Http.NetworkError


suite : Test
suite =
    describe "an unrecognised URL"
        [ test "is retried while the statistics are still loading" <|
            \_ ->
                App.initAt "/definitely-not-a-route/"
                    |> App.expectEffect "PostponeUpdateByUrlEffect" isPostpone
        , test "is not retried once the statistics have settled" <|
            \_ ->
                App.initAtWithStats settled "/definitely-not-a-route/"
                    |> App.expectEffect "no PostponeUpdateByUrlEffect" (isPostpone >> not)
        , test "raises a notification instead" <|
            \_ ->
                App.initAtWithStats settled "/definitely-not-a-route/"
                    |> App.expectEffect "NotificationEffect" isNotification
        , test "leaves the user on the page they were on" <|
            -- `model.url` still holds the bad path, because that is what the address
            -- bar says and nothing redirects; what must not change is the page.
            \_ ->
                App.initAtWithStats settled "/definitely-not-a-route/"
                    |> App.model
                    |> .page
                    |> Expect.equal Model.Home
        ]
