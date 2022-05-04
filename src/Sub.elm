module Sub exposing (subscriptions)

import Browser.Navigation as Nav
import Locale.Subscriptions as Locale
import Model exposing (Model, Msg(..))


subscriptions : Model Nav.Key -> Sub Msg
subscriptions model =
    Locale.subscriptions model.locale
        |> Sub.map LocaleMsg
