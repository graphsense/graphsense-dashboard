module Sub.Locale exposing (subscriptions)

import Browser.Events
import Model.Locale exposing (Model, State(..))
import Msg.Locale exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mapping of
        Transition _ _ _ ->
            Browser.Events.onAnimationFrameDelta RuntimeTick

        _ ->
            Sub.none
