module Scenario.StatsFailureTest exposing (suite)

{-| `/stats` is asked at boot, and an opened graph waits for the answer (it says
which networks the backend serves). A transient error is retried like any other
request (the retry token every API effect gets); once the retries run out, or
the error is not transient, the statistics must end up `Failure` -- otherwise
they stay `Loading` forever and so does the graph waiting for them.
-}

import Dict
import Effect.Api
import Expect
import Http
import Json.Encode
import Model exposing (Msg(..))
import RemoteData
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)


{-| A failed `/stats`. With a retry token, as the app sends it; the harness
does not assign tokens, so it passes one explicitly.
-}
statsFailed : Maybe String -> Http.Error -> Msg
statsFailed token err =
    Err ( err, Dict.empty, Effect.Api.GetStatisticsEffect BrowserGotStatistics )
        |> BrowserGotResponseWithHeaders token


booting : App
booting =
    App.initAtWithStats RemoteData.Loading "/pathfinder"


oneBtcAddressGs : ( String, Json.Encode.Value )
oneBtcAddressGs =
    ( "btc.gs"
    , Json.Encode.list identity
        [ Json.Encode.string "pathfinder"
        , Json.Encode.string "1"
        , Json.Encode.string "btc"
        , Json.Encode.list identity
            [ Json.Encode.list identity
                [ Json.Encode.list Json.Encode.string [ "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" ]
                , Json.Encode.float 0
                , Json.Encode.float 0
                , Json.Encode.bool False
                ]
            ]
        , Json.Encode.list identity []
        , Json.Encode.list identity []
        , Json.Encode.list identity []
        ]
    )


statsState : App -> String
statsState app =
    case (App.model app).stats of
        RemoteData.Loading ->
            "Loading"

        RemoteData.Failure _ ->
            "Failure"

        RemoteData.Success _ ->
            "Success"

        RemoteData.NotAsked ->
            "NotAsked"


suite : Test
suite =
    describe "a failed /stats"
        [ test "a transient error that is being retried keeps the statistics loading" <|
            \_ ->
                booting
                    |> App.step (statsFailed (Just "stats") (Http.BadStatus 502))
                    |> statsState
                    |> Expect.equal "Loading"
        , test "once the retries ran out the statistics are a failure" <|
            \_ ->
                booting
                    |> App.steps (List.repeat 4 (statsFailed (Just "stats") Http.Timeout))
                    |> statsState
                    |> Expect.equal "Failure"
        , test "a permanent error is a failure right away" <|
            \_ ->
                booting
                    |> App.step (statsFailed (Just "stats") (Http.BadStatus 500))
                    |> statsState
                    |> Expect.equal "Failure"
        , test "a graph still opens after /stats failed for good" <|
            \_ ->
                booting
                    |> App.step (BrowserGotCapabilities { networks = [] })
                    |> App.step (statsFailed (Just "stats") (Http.BadStatus 500))
                    |> App.step (BrowserGotDeserializedGS oneBtcAddressGs)
                    |> App.model
                    |> .pathfinder
                    |> .network
                    |> .addresses
                    |> Dict.size
                    |> Expect.equal 1
        ]
