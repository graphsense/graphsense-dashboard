module Update.Graph.Coords exposing (..)

import Config.Graph exposing (addressHeight, entityMinHeight, entityOneAddressHeight, entityTotalWidth, entityWidth, expandHandleWidth)
import Model.Graph.Coords exposing (BBox)


addMargin : BBox -> BBox
addMargin bbox =
    let
        marginX =
            entityTotalWidth / 2

        marginY =
            entityOneAddressHeight / 2
    in
    { x = bbox.x - marginX
    , y = bbox.y - marginY
    , width = bbox.width + marginX * 2
    , height = bbox.height + marginY * 2
    }


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
