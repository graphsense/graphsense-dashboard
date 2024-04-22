module Sub.Pathfinder exposing (subscriptions)

import Browser.Events
import Json.Decode
import Model.Graph exposing (Dragging(..))
import Model.Pathfinder exposing (Model)
import Msg.Pathfinder exposing (Msg(..))
import Ports
import Sub.Graph.Transform as Transform


subscriptions : Model -> Sub Msg
subscriptions model =
    [ case model.dragging of
        NoDragging ->
            Sub.none

        _ ->
            Browser.Events.onMouseUp (Json.Decode.succeed UserReleasesMouseButton)
    , Transform.subscriptions AnimationFrameDeltaForTransform model.transform
    ]
        |> Sub.batch
