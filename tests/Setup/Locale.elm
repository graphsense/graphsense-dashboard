module Setup.Locale exposing (simulateEffects)

import Effect.Locale exposing (Effect(..))
import Msg.Locale exposing (Msg)
import ProgramTest
import SimulatedEffect.Cmd
import SimulatedEffect.Http as Http


simulateEffects : Effect -> ProgramTest.SimulatedEffect Msg
simulateEffects eff =
    case eff of
        NoEffect ->
            SimulatedEffect.Cmd.none

        BatchEffect effs ->
            List.map simulateEffects effs
                |> SimulatedEffect.Cmd.batch

        GetTranslationEffect { url, toMsg } ->
            Http.get
                { url = url
                , expect = Http.expectString toMsg
                }

        GetTimezoneEffect _ ->
            SimulatedEffect.Cmd.none
