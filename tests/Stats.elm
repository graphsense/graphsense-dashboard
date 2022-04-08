module Stats exposing (exampleProgramTest, start)

import Api.Data
import Effect exposing (Effect(..))
import Expect
import Init exposing (init)
import Json.Decode as Dec
import Json.Encode as Enc
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas)
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import Test exposing (..)
import Test.Html.Selector exposing (class, text)
import Update exposing (update)
import View exposing (view)


start : String -> Flags -> ProgramTest (Model ()) Msg Effect
start initialUrl flags =
    ProgramTest.createApplication
        { onUrlChange = BrowserChangedUrl
        , onUrlRequest = UserRequestsUrl
        , init = init
        , update = update
        , view = view
        }
        |> ProgramTest.withBaseUrl initialUrl
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.start flags


exampleProgramTest : Test
exampleProgramTest =
    test "fetch stats on start up" <|
        \() ->
            start "https://app" {}
                |> ProgramTest.simulateHttpOk
                    "GET"
                    "http://localhost:9000/stats"
                    (Api.Data.encodeStats (Api.Data.Stats Nothing Nothing <| Just "the_version")
                        |> Enc.encode 0
                    )
                |> expectViewHas
                    [ text "the_version"
                    ]


simulateEffects : Effect -> ProgramTest.SimulatedEffect Msg
simulateEffects effect =
    case effect of
        NoEffect ->
            SimulatedEffect.Cmd.none

        NavLoadEffect _ ->
            SimulatedEffect.Cmd.none

        NavPushUrlEffect _ ->
            SimulatedEffect.Cmd.none

        GetStatisticsEffect ->
            SimulatedEffect.Http.get
                { url = "http://localhost:9000/stats"
                , expect =
                    SimulatedEffect.Http.expectJson BrowserGotStatistics Api.Data.statsDecoder
                }
