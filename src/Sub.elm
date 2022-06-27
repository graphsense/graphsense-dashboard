module Sub exposing (subscriptions)

import Browser.Events
import Browser.Navigation as Nav
import Model exposing (Model, Msg(..))
import Sub.Graph as Graph
import Sub.Locale as Locale


subscriptions : Model Nav.Key -> Sub Msg
subscriptions model =
    [ Locale.subscriptions model.locale
        |> Sub.map LocaleMsg
    , Graph.subscriptions model.graph
        |> Sub.map GraphMsg
    , Browser.Events.onResize
        BrowserChangedWindowSize
    ]
        |> Sub.batch
