module Scenario.LiteUnavailableTest exposing (suite)

{-| The lite blockchains did not answer: the API left them out of a merged
answer and named the lite service in x-ikn-backends-declined, or answered a
request on a lite blockchain 503. The core blockchains still work, so the app
says so in one notice instead of an error per request, does not retry the
503, and forgets the outage once the lite blockchains answer again.
-}

import Dict exposing (Dict)
import Effect.Api
import Expect
import Http
import Model exposing (Msg(..))
import Model.Notification as Notification
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)


declined : Dict String String
declined =
    Dict.fromList [ ( "x-ikn-backends-declined", "http://external-backend-adapter:9876" ) ]


servedByLite : Dict String String
servedByLite =
    Dict.fromList [ ( "x-served-by", "external-backend" ) ]


{-| A merged answer (/capabilities stands for /stats and /search as well).
-}
mergedAnswer : Dict String String -> Msg
mergedAnswer headers =
    Ok ( headers, BrowserGotCapabilities { networks = [] } )
        |> BrowserGotResponseWithHeaders Nothing


liteAnswer : Msg
liteAnswer =
    Ok ( servedByLite, NoOp )
        |> BrowserGotResponseWithHeaders Nothing


{-| A failed request for a bnb address, with the retry token the app sends.
-}
addressFailed : Dict String String -> Http.Error -> Msg
addressFailed headers err =
    Err
        ( err
        , headers
        , Effect.Api.GetAddressEffect
            { currency = "bnb"
            , address = "0xd10ed57534dc63f2ea9dc0cb0096086f3cc8fa4d"
            , includeActors = True
            }
            (\_ -> NoOp)
        )
        |> BrowserGotResponseWithHeaders (Just "bnb-address")


start : App
start =
    App.initAt "/pathfinder"


notices : App -> List String
notices app =
    let
        text n =
            case n of
                Notification.Error data ->
                    "error: " ++ data.message

                Notification.Info data ->
                    data.message

                Notification.Success data ->
                    data.message

        collect : Notification.Model -> List String -> List String
        collect queue acc =
            case Notification.peek queue of
                Just n ->
                    collect (Notification.pop queue) (text n :: acc)

                Nothing ->
                    List.reverse acc
    in
    collect (App.model app).notifications []


liteUnavailable : App -> Bool
liteUnavailable app =
    (App.model app).config.liteUnavailable


retried : App -> Bool
retried app =
    (App.model app).statusbar.retries |> Dict.member "bnb-address"


suite : Test
suite =
    describe "the lite blockchains do not answer"
        [ test "a merged answer without them shows one notice, however often it comes" <|
            \_ ->
                start
                    |> App.steps [ mergedAnswer declined, mergedAnswer declined ]
                    |> (\app -> ( liteUnavailable app, notices app ))
                    |> Expect.equal ( True, [ "lite-unavailable-message" ] )
        , test "a complete merged answer shows nothing" <|
            \_ ->
                start
                    |> App.step (mergedAnswer Dict.empty)
                    |> (\app -> ( liteUnavailable app, notices app ))
                    |> Expect.equal ( False, [] )
        , test "a lite request answered 503 shows the notice instead of an error and is not retried" <|
            \_ ->
                start
                    |> App.step (addressFailed servedByLite (Http.BadStatus 503))
                    |> (\app -> ( liteUnavailable app, notices app, retried app ))
                    |> Expect.equal ( True, [ "lite-unavailable-message" ], False )
        , test "a 503 of the core is still retried" <|
            \_ ->
                start
                    |> App.step (addressFailed Dict.empty (Http.BadStatus 503))
                    |> (\app -> ( liteUnavailable app, retried app ))
                    |> Expect.equal ( False, True )
        , test "a complete merged answer ends the outage" <|
            \_ ->
                start
                    |> App.steps [ mergedAnswer declined, mergedAnswer Dict.empty ]
                    |> liteUnavailable
                    |> Expect.equal False
        , test "an answer of the lite service ends the outage" <|
            \_ ->
                start
                    |> App.steps [ mergedAnswer declined, liteAnswer ]
                    |> liteUnavailable
                    |> Expect.equal False
        , test "any other answer leaves it as it is" <|
            \_ ->
                start
                    |> App.steps [ mergedAnswer declined, BrowserGotResponseWithHeaders Nothing (Ok ( Dict.empty, NoOp )) ]
                    |> liteUnavailable
                    |> Expect.equal True
        ]
