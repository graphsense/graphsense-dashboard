module Sub.Graph exposing (subscriptions)

import Browser.Events
import Json.Decode
import Model.Graph exposing (Model)
import Model.Graph.Transform exposing (Dragging(..))
import Msg.Graph exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    [ case model.transform.dragging of
        Dragging _ _ ->
            Browser.Events.onMouseUp (Json.Decode.succeed UserReleasesMouseButton)

        _ ->
            Sub.none
    ]
        |> Sub.batch
