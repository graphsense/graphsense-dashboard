module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Config exposing (config)
import Effect exposing (perform)
import Init exposing (init)
import Locale.Model as Locale
import Locale.View as Locale
import Model exposing (Flags, Model, Msg(..))
import Sub exposing (subscriptions)
import Tuple
import Update exposing (update)
import View exposing (view)


main : Program Flags (Model Nav.Key) Msg
main =
    let
        performEffect ( model, effect ) =
            ( model, perform model.key model.user.apiKey effect )
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
