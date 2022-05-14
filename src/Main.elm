module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Config exposing (config)
import Effect exposing (perform)
import Init exposing (init)
import Model exposing (Flags, Model, Msg(..))
import Model.Locale as Locale
import Sub exposing (subscriptions)
import Tuple exposing (..)
import Update exposing (update, updateByUrl)
import View exposing (view)
import View.Locale as Locale


main : Program Flags (Model Nav.Key) Msg
main =
    let
        performEffect ( model, effects ) =
            ( model, List.map (perform model.key model.user.apiKey) effects |> Cmd.batch )

        uc =
            { defaultColor = config.theme.graph.defaultColor
            , colorScheme = config.theme.graph.colorScheme
            }
    in
    Browser.application
        { init =
            \flags url key ->
                let
                    ( model, effects ) =
                        init flags url key
                in
                updateByUrl uc url model
                    |> mapSecond ((++) effects)
                    |> performEffect
        , update =
            \msg model ->
                update
                    uc
                    msg
                    model
                    |> performEffect
        , view =
            \model ->
                view
                    model.config
                    model
        , subscriptions = subscriptions
        , onUrlChange = BrowserChangedUrl
        , onUrlRequest = UserRequestsUrl
        }
