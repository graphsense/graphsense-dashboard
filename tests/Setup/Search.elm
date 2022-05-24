module Setup.Search exposing (simulateEffects)

import Api
import Api.Data
import Api.Request.General
import Effect.Search exposing (Effect(..))
import Msg.Search exposing (Msg(..))
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http as Http
import SimulatedEffect.Task as Task


simulateEffects : Effect -> ProgramTest.SimulatedEffect Msg
simulateEffects eff =
    case eff of
        SearchEffect { query, currency, limit, toMsg } ->
            SimulatedEffect.Cmd.none

        BounceEffect delay msg ->
            Task.succeed ()
                |> Task.perform (\_ -> msg)

        CancelEffect ->
            SimulatedEffect.Cmd.none
