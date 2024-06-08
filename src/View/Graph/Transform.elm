module View.Graph.Transform exposing (viewBox)

import Config.Graph exposing (addressHeight, entityMinHeight, entityWidth, expandHandleWidth)
import Model.Graph.Transform exposing (..)
import Number.Bounded as Bounded


viewBox : { width : Float, height : Float } -> Model comparable -> String
viewBox { width, height } mo =
    getCurrent mo
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
