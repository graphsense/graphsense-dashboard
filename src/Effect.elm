module Effect exposing (Effect(..), batch, n, perform)

import Api
import Api.Request.General
import Browser.Navigation as Nav
import Locale.Effect
import Msg exposing (Msg(..))


type Effect
    = NoEffect
    | NavLoadEffect String
    | NavPushUrlEffect String
    | GetStatisticsEffect
    | BatchedEffects (List Effect)
    | LocaleEffect Locale.Effect.Effect


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )


batch : List Effect -> Effect
batch effs =
    BatchedEffects effs


perform : Nav.Key -> Effect -> Cmd Msg
perform key effect =
    case effect of
        NoEffect ->
            Cmd.none

        NavLoadEffect url ->
            Nav.load url

        NavPushUrlEffect url ->
            Nav.pushUrl key url

        GetStatisticsEffect ->
            Api.Request.General.getStatistics
                |> Api.send BrowserGotStatistics

        BatchedEffects effs ->
            List.map (perform key) effs
                |> Cmd.batch

        LocaleEffect eff ->
            Locale.Effect.perform eff
                |> Cmd.map LocaleMsg
