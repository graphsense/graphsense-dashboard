module Effect exposing (Effect(..), n, perform)

import Browser.Navigation as Nav
import Msg exposing (Msg)


type Effect
    = NoEffect
    | NavLoadEffect String
    | NavPushUrlEffect Nav.Key String


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        NavLoadEffect url ->
            Nav.load url

        NavPushUrlEffect key url ->
            Nav.pushUrl key url
