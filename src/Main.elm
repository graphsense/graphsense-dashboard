module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Config exposing (config)
import Effect exposing (perform)
import Init exposing (init)
import Model exposing (Flags, Model, Msg(..))
import Model.Locale as Locale
import Sub exposing (subscriptions)
import Tuple
import Update exposing (update)
import View exposing (view)
import View.Locale as Locale


main : Program Flags (Model Nav.Key) Msg
main =
    let
        performEffect ( model, effects ) =
            ( model, List.map (perform model.key model.user.apiKey) effects |> Cmd.batch )
    in
    Browser.application
        { init =
            \flags url key ->
                init flags url key
                    |> performEffect
        , update =
            \msg model ->
                update msg model
                    |> performEffect
        , view =
            \model ->
                view
                    { theme = config.theme
                    , locale = model.locale
                    }
                    model
        , subscriptions = subscriptions
        , onUrlChange = BrowserChangedUrl
        , onUrlRequest = UserRequestsUrl
        }
