module Main exposing (main)

import Browser
import Effect exposing (perform)
import Init exposing (init)
import Model exposing (Flags, Model)
import Msg exposing (Msg(..))
import Sub exposing (subscriptions)
import Tuple
import Update exposing (update)
import View exposing (view)


main : Program Flags Model Msg
main =
    Browser.application
        { init =
            \flags url key ->
                init flags url key
                    |> Tuple.mapSecond perform
        , update =
            \msg model ->
                update msg model
                    |> Tuple.mapSecond perform
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = BrowserChangedUrl
        , onUrlRequest = UserRequestsUrl
        }
