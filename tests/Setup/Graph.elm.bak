module Setup.Graph exposing (simulateEffects)

import Effect.Graph exposing (Effect(..))
import Msg.Graph exposing (Msg)
import ProgramTest
import SimulatedEffect.Cmd


simulateEffects : Effect -> ProgramTest.SimulatedEffect Msg
simulateEffects eff =
    case eff of
        PluginEffect _ ->
            SimulatedEffect.Cmd.none

        GetAddressEffect _ ->
            SimulatedEffect.Cmd.none

        GetEntityEffect _ ->
            SimulatedEffect.Cmd.none

        NavPushRouteEffect _ ->
            SimulatedEffect.Cmd.none

        GetSvgElementEffect ->
            SimulatedEffect.Cmd.none

        GetEntityForAddressEffect _ ->
            SimulatedEffect.Cmd.none

        GetEntityNeighborsEffect _ ->
            SimulatedEffect.Cmd.none

        GetAddressNeighborsEffect _ ->
            SimulatedEffect.Cmd.none

        GetAddressTxsEffect _ ->
            SimulatedEffect.Cmd.none
