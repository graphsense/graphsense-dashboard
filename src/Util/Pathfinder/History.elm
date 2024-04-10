module Util.Pathfinder.History exposing (..)

import Model.Pathfinder exposing (Model)
import Msg.Pathfinder exposing (Msg(..))


shallPushHistory : Msg -> Model -> Bool
shallPushHistory msg model =
    case msg of
        UserClickedGraph _ ->
            False

        SearchMsg _ ->
            False

        UserPushesLeftMouseButtonOnGraph _ ->
            False

        UserMovesMouseOnGraph _ ->
            False

        UserWheeledOnGraph _ _ _ ->
            False

        PluginMsg _ ->
            False

        _ ->
            False
