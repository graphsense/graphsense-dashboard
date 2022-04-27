module Search.Setup exposing (simulateEffects)

import Api
import Api.Data
import Api.Request.General
import ProgramTest exposing (ProgramTest)
import Search.Effect exposing (Effect(..))
import Search.Msg exposing (Msg(..))
import SimulatedEffect.Cmd
import SimulatedEffect.Http as Http
import SimulatedEffect.Task as Task


simulateEffects : Effect -> ProgramTest.SimulatedEffect Msg
simulateEffects eff =
    case eff of
        NoEffect ->
            SimulatedEffect.Cmd.none

        BatchEffect effs ->
            List.map simulateEffects effs
                |> SimulatedEffect.Cmd.batch

        SearchEffect { query, currency, limit, toMsg } ->
            Api.Request.General.search query currency limit
                |> Api.effect toMsg

        BounceEffect delay msg ->
            Task.succeed ()
                |> Task.perform (\_ -> msg)

        CancelEffect ->
            SimulatedEffect.Cmd.none
