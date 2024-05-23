module Setup.Search exposing (simulateEffects)

import Effect.Search exposing (Effect(..))
import Msg.Search exposing (Msg)
import ProgramTest
import SimulatedEffect.Cmd
import SimulatedEffect.Task as Task


simulateEffects : Effect -> ProgramTest.SimulatedEffect Msg
simulateEffects eff =
    case eff of
        SearchEffect _ ->
            SimulatedEffect.Cmd.none

        BounceEffect _ msg ->
            Task.succeed ()
                |> Task.perform (\_ -> msg)

        CancelEffect ->
            SimulatedEffect.Cmd.none
