module Sub.Graph.Transform exposing (subscriptions)

import Browser.Events as Browser
import Model.Graph.Transform exposing (..)


subscriptions : (Float -> msg) -> Model id -> Sub msg
subscriptions onAnimationFrameDelta model =
    case model.state of
        Transitioning _ ->
            Browser.onAnimationFrameDelta onAnimationFrameDelta

        Settled _ ->
            Sub.none
