module Locale.Setup exposing (simulateEffects)

import Locale.Effect exposing (Effect(..))
import Locale.Msg exposing (Msg(..))
import ProgramTest exposing (ProgramTest)
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

        GetTimezoneEffect toMsg ->
            SimulatedEffect.Cmd.none
