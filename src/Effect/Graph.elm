module Effect.Graph exposing (Effect(..), perform)

import Browser.Dom
import Msg.Graph exposing (Msg(..))
import Task


type Effect
    = GetSvgElementEffect


perform : Effect -> Cmd Msg
perform eff =
    case eff of
        GetSvgElementEffect ->
            Browser.Dom.getElement "graph"
                |> Task.attempt BrowserGotSvgElement
