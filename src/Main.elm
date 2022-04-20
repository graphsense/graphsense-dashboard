module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Config exposing (config)
import Effect exposing (perform)
import Init exposing (init)
import Locale.Model as Locale
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import Sub exposing (subscriptions)
import Tuple
import Update exposing (update)
import View exposing (view)


main : Program Flags (Model Nav.Key) Msg
main =
    let
        performEffect ( model, effect ) =
            ( model, perform model.key effect )

        vc =
            { theme = config.theme
            , getString = identity
            }
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
                    { vc | getString = Locale.getString model.locale }
                    model
        , subscriptions = subscriptions
        , onUrlChange = BrowserChangedUrl
        , onUrlRequest = UserRequestsUrl
        }
