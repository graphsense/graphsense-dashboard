module Effect exposing (Effect(..), n, perform)

import Api
import Api.Request.General
import Browser.Navigation as Nav
import Msg exposing (Msg(..))


type Effect
    = NoEffect
    | NavLoadEffect String
    | NavPushUrlEffect String
    | GetStatisticsEffect


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )


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
