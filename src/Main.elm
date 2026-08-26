module Main exposing (main)

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Navigation as Nav
import Config.UserSettings exposing (default)
import Effect exposing (perform)
import Init exposing (init)
import Init.Locale as Locale
import Model exposing (Flags, Model, Msg(..))
import Sub exposing (subscriptions)
import Tuple exposing (..)
import Update exposing (update, updateByUrl)
import Update.Notification as Notification
import Update.Statusbar as Statusbar
import View exposing (view)


main : Program Flags (Model Nav.Key) Msg
main =
    let
        performEffect ( model, effects ) =
            Notification.notificationsFromEffects model effects
                |> uncurry Statusbar.messagesFromEffects
                |> mapSecond
                    (List.map
                        (\( statusbarToken, eff ) ->
                            perform model statusbarToken model.user.apiKey eff
                        )
                    )
                |> mapSecond Cmd.batch

        uc =
            { locale = Locale.init (default "en") |> first
            , size = Nothing
            , abuseConcepts = []
            , allConcepts = []
            }
    in
    Browser.application
        { init =
            \flags url key ->
                let
                    ( model, effects ) =
                        init uc flags url key
                in
                updateByUrl uc url model
                    |> mapSecond ((++) effects)
                    |> performEffect
        , update =
            \msg model ->
                update
                    { uc
                        | locale = model.config.locale
                        , size = model.config.size
                        , abuseConcepts = model.config.abuseConcepts
                        , allConcepts = model.config.allConcepts
                    }
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
