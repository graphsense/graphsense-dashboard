module Model.Pathfinder.Overview exposing
    ( Model, Visibility(..), Layout
    , init, mapWidth, mapHeight
    , layout, toMap, toGraph, isShown
    )

{-| The overview map (minimap) of the Pathfinder graph: which part of the graph
the screen shows, and a way to jump elsewhere.

Positions are in graph pixels, the space the transform works in (graph units
times `Model.Pathfinder.unit`). The map scales to the graph alone, not to the
viewport, so it only changes when the graph does: panning moves the viewport
box, not the dots.

@docs Model, Visibility, Layout
@docs init, mapWidth, mapHeight
@docs layout, toMap, toGraph, isShown

-}

import Model.Graph.Coords exposing (BBox)


{-| `Auto`: shown while part of the graph is off screen. `Hidden`: the user
hid it. `Shown`: the user brought it back, so it stays.
-}
type Visibility
    = Auto
    | Hidden
    | Shown


type alias Model =
    { visibility : Visibility

    -- a drag on the map is under way
    , dragging : Bool
    }


init : Model
init =
    { visibility = Auto
    , dragging = False
    }


mapWidth : Float
mapWidth =
    206


mapHeight : Float
mapHeight =
    130


{-| Map pixel = offset + (graph pixel - origin) \* scale.
-}
type alias Layout =
    { scale : Float
    , originX : Float
    , originY : Float
    , offsetX : Float
    , offsetY : Float
    }


{-| Fits the points, plus a margin of `margin` graph pixels, into the map,
centred. Nothing for no points.
-}
layout : Float -> List { x : Float, y : Float } -> Maybe Layout
layout margin points =
    case ( List.map .x points, List.map .y points ) of
        ( x :: xs, y :: ys ) ->
            let
                x0 =
                    List.foldl min x xs - margin

                x1 =
                    List.foldl max x xs + margin

                y0 =
                    List.foldl min y ys - margin

                y1 =
                    List.foldl max y ys + margin

                scale =
                    min (mapWidth / max 1 (x1 - x0)) (mapHeight / max 1 (y1 - y0))
            in
            Just
                { scale = scale
                , originX = x0
                , originY = y0
                , offsetX = (mapWidth - (x1 - x0) * scale) / 2
                , offsetY = (mapHeight - (y1 - y0) * scale) / 2
                }

        _ ->
            Nothing


toMap : Layout -> { x : Float, y : Float } -> { x : Float, y : Float }
toMap l p =
    { x = l.offsetX + (p.x - l.originX) * l.scale
    , y = l.offsetY + (p.y - l.originY) * l.scale
    }


toGraph : Layout -> { x : Float, y : Float } -> { x : Float, y : Float }
toGraph l p =
    { x = (p.x - l.offsetX) / l.scale + l.originX
    , y = (p.y - l.offsetY) / l.scale + l.originY
    }


{-| Whether to show the map: in `Auto`, while a point lies outside the
viewport (by more than `inset` graph pixels at its edges).
-}
isShown : Float -> Model -> BBox -> List { x : Float, y : Float } -> Bool
isShown inset model viewport points =
    not (List.isEmpty points)
        && (case model.visibility of
                Hidden ->
                    False

                Shown ->
                    True

                Auto ->
                    List.any
                        (\p ->
                            (p.x < viewport.x + inset)
                                || (p.x > viewport.x + viewport.width - inset)
                                || (p.y < viewport.y + inset)
                                || (p.y > viewport.y + viewport.height - inset)
                        )
                        points
           )
