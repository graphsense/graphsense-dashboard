module View.Graph.Transform exposing (background, viewBox)

import Model.Graph.Transform exposing (Model, coordsToBBox, getCurrent)
import Svg.Styled exposing (Attribute, Svg, rect)
import Svg.Styled.Attributes exposing (fill, height, width, x, y)


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


background : List (Attribute msg) -> { a | width : Float, height : Float } -> Model comparable -> Svg msg
background attrs viewport mo =
    getCurrent mo
        |> coordsToBBox viewport
        |> (\bbox ->
                rect
                    ([ x <| String.fromFloat bbox.x
                     , y <| String.fromFloat bbox.y
                     , width <| String.fromFloat bbox.width
                     , height <| String.fromFloat bbox.height
                     , fill "transparent"
                     ]
                        ++ attrs
                    )
                    []
           )
