module Sub exposing (subscriptions)

import Browser.Navigation as Nav
import Model exposing (Model)
import Msg exposing (Msg)


subscriptions : Model Nav.Key -> Sub Msg
subscriptions _ =
    Sub.none
