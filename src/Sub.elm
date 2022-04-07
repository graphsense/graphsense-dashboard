module Sub exposing (subscriptions)

import Model exposing (Model)
import Msg exposing (Msg)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
