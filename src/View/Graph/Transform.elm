module View.Graph.Transform exposing (viewBox)

import Model.Graph.Transform as GTransform
import Number.Bounded as Bounded


viewBox : { width : Float, height : Float } -> GTransform.Model comparable -> String
viewBox { width, height } mo =
    GTransform.getCurrent mo
        |> (\model ->
                let
                    z =
                        Bounded.value model.z
                in
                [ model.x - width / 2 * z
                , model.y - height / 2 * z
                , max 0 <| width * z
                , max 0 <| height * z
                ]
                    |> List.map String.fromFloat
                    |> String.join " "
           )
