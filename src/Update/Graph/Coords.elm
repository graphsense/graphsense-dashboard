module Update.Graph.Coords exposing (draggingToClick, mergeBoundingBoxes)

import Model.Graph.Coords as Coords exposing (BBox, Coords)


{-| A drag that moved less than 2px is treated as a click rather than a drag.
-}
draggingToClick : Coords -> Coords -> Bool
draggingToClick start current =
    Coords.betrag start current < 2


mergeBoundingBoxes : BBox -> BBox -> BBox
mergeBoundingBoxes a b =
    let
        x1 =
            min a.x b.x

        y1 =
            min a.y b.y

        x2 =
            max (a.x + a.width) (b.x + b.width)

        y2 =
            max (a.y + a.height) (b.y + b.height)
    in
    { x = x1
    , y = y1
    , width = x2 - x1
    , height = y2 - y1
    }
