module Sub.Graph exposing (subscriptions)

import Browser.Events
import Json.Decode
import Model.Graph exposing (Dragging(..), Model)
import Msg.Graph exposing (Msg(..))
import Ports
import Sub.Graph.Transform as Transform


subscriptions : Model -> Sub Msg
subscriptions model =
    [ case model.dragging of
        NoDragging ->
            Sub.none

        _ ->
            Browser.Events.onMouseUp (Json.Decode.succeed UserReleasesMouseButton)
    , Ports.deserialized PortDeserializedGS
    , Browser.Events.onKeyUp
        (Json.Decode.field "key" Json.Decode.string
            |> Json.Decode.map
                (\str ->
                    if str == "Escape" then
                        UserPressesEscape

                    else if str == "Delete" then
                        UserPressesDelete

                    else
                        NoOp
                )
        )
    , Transform.subscriptions model.transform
    ]
        |> Sub.batch
