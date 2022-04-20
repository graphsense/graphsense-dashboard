module Locale.Subscriptions exposing (subscriptions)

import Browser.Events
import Locale.Model exposing (Model, State(..))
import Locale.Msg exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mapping of
        Transition _ _ _ ->
            Browser.Events.onAnimationFrameDelta RuntimeTick

        _ ->
            Sub.none
