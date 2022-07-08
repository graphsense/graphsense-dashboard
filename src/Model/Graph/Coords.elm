module Model.Graph.Coords exposing (..)


type alias Coords =
    { x : Float, y : Float }


type alias BBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


betrag : Coords -> Coords -> Float
betrag start current =
    (current.x - start.x)
        + (current.y - start.y)
        |> (\x -> x ^ 2)
        |> sqrt


relativeToGraph : Maybe BBox -> Coords -> Coords
relativeToGraph bbox coords =
    bbox
        |> Maybe.map
            (\{ x, y } ->
                { x = coords.x - x
                , y = coords.y - y
                }
            )
        |> Maybe.withDefault coords
