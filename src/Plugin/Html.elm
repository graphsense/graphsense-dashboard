module Plugin.Html exposing (iterate)

import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Json.Encode exposing (Value)
import Msg.Graph exposing (Msg(..))
import Plugin exposing (PID, Plugin)


iterate :
    Dict PID Plugin
    -> (Plugin -> Value -> List (Html Value))
    -> Dict String Value
    -> List (List (Html Msg))
iterate plugins fun states =
    Plugin.iterate plugins fun states
        |> List.map (\( pid, list ) -> List.map (Html.map (PluginMsg pid)) list)
