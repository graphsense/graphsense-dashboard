module Sub exposing (subscriptions)

import Browser.Events
import Browser.Navigation as Nav
import Model exposing (Model, Msg(..))
import Sub.Graph as Graph
import Sub.Locale as Locale
import Time


subscriptions : Model Nav.Key -> Sub Msg
subscriptions model =
    [ Locale.subscriptions model.locale
        |> Sub.map LocaleMsg
    , Graph.subscriptions model.graph
        |> Sub.map GraphMsg
    , Browser.Events.onResize
        BrowserChangedWindowSize
    , case model.user.auth of
        Model.Authorized auth ->
            case auth.requestLimit of
                Model.Limited { remaining, reset } ->
                    if reset > 0 && remaining < Model.showResetCounterAtRemaining then
                        Time.every 1000 TimeUpdateReset

                    else
                        Sub.none

                _ ->
                    Sub.none

        _ ->
            Sub.none
    ]
        |> Sub.batch
