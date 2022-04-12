module Stats exposing (start, statsTest)

import Api
import Api.Data
import Api.Request.General
import Effect exposing (Effect(..))
import Expect
import Init exposing (init)
import Json.Decode as Dec
import Json.Encode as Enc
import Mockup.Stats
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas)
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import Test exposing (..)
import Test.Html.Selector exposing (class, text)
import Update exposing (update)
import Urls exposing (baseUrl)
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


statsTest : Test
statsTest =
    test "fetch stats on start up" <|
        \() ->
            start baseUrl {}
                |> ProgramTest.ensureHttpRequestWasMade "GET" (Api.baseUrl ++ "/stats")
                |> ProgramTest.simulateHttpOk
                    "GET"
                    (Api.baseUrl ++ "/stats")
                    Mockup.Stats.statsEncoded
                |> expectViewHas
                    [ text <| Mockup.Stats.stats.version
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
            Api.Request.General.getStatistics
                |> Api.effect BrowserGotStatistics

        BatchedEffects effs ->
            List.map simulateEffects effs
                |> SimulatedEffect.Cmd.batch
