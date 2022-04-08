module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Effect exposing (perform)
import Init exposing (init)
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
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = BrowserChangedUrl
        , onUrlRequest = UserRequestsUrl
        }
