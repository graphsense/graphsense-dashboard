module View.Graph.Transform exposing (viewBox)

import Model.Graph.Transform as GTransform


viewBox : { width : Float, height : Float } -> GTransform.Model comparable -> String
viewBox viewport mo =
    GTransform.getCurrent mo
        |> GTransform.coordsToBBox viewport
        |> (\bbox ->
                [ bbox.x
                , bbox.y
                , bbox.width
                , bbox.height
                ]
                    |> List.map String.fromFloat
                    |> String.join " "
           )
