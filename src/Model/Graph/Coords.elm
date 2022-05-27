module Model.Graph.Coords exposing (..)


type alias Coords =
    { x : Float, y : Float }


betrag : Coords -> Coords -> Float
betrag start current =
    (current.x - start.x)
        + (current.y - start.y)
        |> (\x -> x ^ 2)
        |> sqrt
