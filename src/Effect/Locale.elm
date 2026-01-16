module Effect.Locale exposing (Effect(..), getTranslationEffect, perform)

import Http
import Msg.Locale exposing (Msg(..))
import Task
import Time
import Yaml.Decode exposing (dict, fromString, string)


type Effect
    = GetTranslationEffect { url : String, toMsg : Result Http.Error String -> Msg }
    | GetTimezoneEffect (Time.Zone -> Msg)


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        GetTranslationEffect { url, toMsg } ->
            Http.get
                { url = url
                , expect = Http.expectString toMsg
                }

        GetTimezoneEffect toMsg ->
            Time.here
                |> Task.perform toMsg


getTranslationEffect : String -> Effect
getTranslationEffect locale =
    { url = "/lang/" ++ locale ++ ".yaml?{{CACHE_BUST}}"
    , toMsg =
        Result.andThen
            (fromString (dict string)
                >> Result.mapError toHttpError
            )
            >> BrowserLoadedTranslation locale
    }
        |> GetTranslationEffect


toHttpError : Yaml.Decode.Error -> Http.Error
toHttpError err =
    case err of
        Yaml.Decode.Parsing e ->
            "Error when parsing YAML: " ++ e |> Http.BadBody

        Yaml.Decode.Decoding e ->
            "Error when decoding YAML: " ++ e |> Http.BadBody
