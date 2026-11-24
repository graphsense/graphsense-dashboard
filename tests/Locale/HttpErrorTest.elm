module Locale.HttpErrorTest exposing (suite)

{-| The texts the statusbar, dialogs and the statistics page show for a failed
request. gs-rest answers 408 when a query runs over its time budget (the links
between two busy clusters) and 503 while its database is down for a moment;
both used to read "bad status: 408" / "bad status: 503".
-}

import Expect
import Http
import Support.Env as Env
import Test exposing (Test, describe, test)
import View.Locale as Locale


text : Http.Error -> String
text =
    Locale.httpErrorToString Env.viewConfig.locale


suite : Test
suite =
    describe "View.Locale.httpErrorToString"
        [ test "a 408 is a request timeout" <|
            \_ -> text (Http.BadStatus 408) |> Expect.equal "request timeout"
        , test "a 503 is a temporarily unavailable service" <|
            \_ -> text (Http.BadStatus 503) |> Expect.equal "service temporarily unavailable"
        , test "a status without its own text still names the code" <|
            \_ -> text (Http.BadStatus 418) |> Expect.equal "bad status: 418"
        ]
