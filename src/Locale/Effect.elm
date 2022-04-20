module Locale.Effect exposing (Effect(..), n, perform)

import Http
import Locale.Msg exposing (Msg(..))
import Yaml.Decode exposing (dict, fromString, string)


type Effect
    = NoEffect
    | GetTranslationEffect String


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        GetTranslationEffect locale ->
            Http.get
                { url = "/lang/" ++ locale ++ ".yaml"
                , expect =
                    Http.expectString
                        (Result.andThen
                            (fromString (dict string)
                                >> Result.mapError toHttpError
                            )
                            >> BrowserLoadedTranslation locale
                        )
                }


toHttpError : Yaml.Decode.Error -> Http.Error
toHttpError err =
    case err of
        Yaml.Decode.Parsing e ->
            "Error when parsing YAML: " ++ e |> Http.BadBody

        Yaml.Decode.Decoding e ->
            "Error when decoding YAML: " ++ e |> Http.BadBody
