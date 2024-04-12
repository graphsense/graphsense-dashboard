module Model.Graph.Coords exposing (..)

import List.Nonempty as NList


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


avg : NList.Nonempty Coords -> Coords
avg coords =
    { x = (NList.map .x coords |> NList.toList |> List.sum) / toFloat (NList.length coords)
    , y = (NList.map .y coords |> NList.toList |> List.sum) / toFloat (NList.length coords)
    }
