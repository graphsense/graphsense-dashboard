module Setup exposing (start)

import Api
import Api.Request.General
import Effect exposing (Effect(..))
import Init exposing (init)
import Locale.Init as Locale
import Locale.Setup as Locale
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http as Http
import Theme.Theme as Theme
import Tuple exposing (first)
import Update exposing (update)
import View exposing (view)


start : String -> Flags -> ProgramTest (Model ()) Msg Effect
start initialPath flags =
    ProgramTest.createApplication
        { onUrlChange = BrowserChangedUrl
        , onUrlRequest = UserRequestsUrl
        , init = init
        , update = update
        , view =
            view
                { theme = Theme.default
                , locale =
                    Locale.init
                        { locale = flags.locale
                        }
                        |> first
                }
        }
        |> ProgramTest.withBaseUrl ("http://foo.bar" ++ initialPath)
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.start flags


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

        LocaleEffect eff ->
            Locale.simulateEffects eff
                |> SimulatedEffect.Cmd.map LocaleMsg
