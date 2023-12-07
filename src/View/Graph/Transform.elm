module View.Graph.Transform exposing (viewBox)

import Config.Graph exposing (addressHeight, entityMinHeight, entityWidth, expandHandleWidth)
import Model.Graph.Transform exposing (..)


viewBox : { width : Float, height : Float } -> Model -> String
viewBox { width, height } mo =
    getCurrent mo
        |> (\model ->
                [ model.x - width / 2 * model.z
                , model.y - height / 2 * model.z
                , max 0 <| width * model.z
                , max 0 <| height * model.z
                ]
                    |> List.map String.fromFloat
                    |> List.intersperse " "
                    |> String.concat
           )
