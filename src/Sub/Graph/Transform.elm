module Sub.Graph.Transform exposing (subscriptions)

import Browser.Events as Browser
import Model.Graph.Transform exposing (..)
import Msg.Graph exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Transitioning _ ->
            Browser.onAnimationFrameDelta AnimationFrameDeltaForTransform

        Settled _ ->
            Sub.none
