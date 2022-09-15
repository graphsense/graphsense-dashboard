module Plugin.Sub exposing (subscriptions)

import Json.Decode
import Plugin.Model
import Plugin.Msg


subscriptions : ((( String, Json.Decode.Value ) -> Plugin.Msg.Msg) -> Sub Plugin.Msg.Msg) -> Plugin.Model.ModelState -> Sub Plugin.Msg.Msg
subscriptions inPort state =
    [ 
    inPort
        (\( namespace, value ) ->
            case namespace of
                _ ->
                    Plugin.Msg.NoOp
        )
    ]
        |> Sub.batch
