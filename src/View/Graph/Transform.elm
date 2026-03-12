module View.Graph.Transform exposing (viewBox)

import Model.Graph.Transform exposing (Model, coordsToBBox, getCurrent)


viewBox : { a | width : Float, height : Float } -> Model comparable -> String
viewBox viewport mo =
    getCurrent mo
        |> coordsToBBox viewport
        |> (\bbox ->
                [ bbox.x
                , bbox.y
                , bbox.width
                , bbox.height
                ]
                    |> List.map String.fromFloat
                    |> String.join " "
           )
