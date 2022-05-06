module Sub exposing (subscriptions)

import Browser.Navigation as Nav
import Model exposing (Model, Msg(..))
import Sub.Locale as Locale


subscriptions : Model Nav.Key -> Sub Msg
subscriptions model =
    Locale.subscriptions model.locale
        |> Sub.map LocaleMsg
