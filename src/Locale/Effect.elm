module Locale.Effect exposing (Effect(..), n, perform)

import Http
import Locale.Msg exposing (Msg)
import Task
import Time


type Effect
    = NoEffect
    | GetTranslationEffect { url : String, toMsg : Result Http.Error String -> Msg }
    | GetTimezoneEffect (Time.Zone -> Msg)
    | BatchEffect (List Effect)


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        GetTranslationEffect { url, toMsg } ->
            Http.get
                { url = url
                , expect = Http.expectString toMsg
                }

        GetTimezoneEffect toMsg ->
            Time.here
                |> Task.perform toMsg

        BatchEffect effs ->
            List.map perform effs
                |> Cmd.batch
