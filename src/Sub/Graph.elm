module Sub.Graph exposing (subscriptions)

import Browser.Events
import Json.Decode
import Model.Graph exposing (Dragging(..), Model)
import Msg.Graph exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    [ case model.dragging of
        NoDragging ->
            Sub.none

        _ ->
            Browser.Events.onMouseUp (Json.Decode.succeed UserReleasesMouseButton)
    ]
        |> Sub.batch
