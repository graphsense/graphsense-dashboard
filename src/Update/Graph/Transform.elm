module Update.Graph.Transform exposing (update, updateByBoundingBox, vector, wheel)

import Config.Graph exposing (addressHeight, entityMinHeight, entityWidth, expandHandleWidth)

import Model.Graph.Coords exposing (BBox, Coords)
import Model.Graph.Transform exposing (..)
import RecordSetter exposing (..)


update : Coords -> Coords -> Model -> Model
update start current transform =
    transform
        |> s_x ((start.x - current.x) * transform.z + transform.x)
        |> s_y ((start.y - current.y) * transform.z + transform.y)


wheel : { width : Float, height : Float } -> Float -> Float -> Float -> Model -> Model
wheel { width, height } x y w model =
    let
        x_ =
            x - width / 2

        y_ =
            y - height / 2

        factor =
            0.005

        z =
            w
                * factor
                |> max -0.9

        moveX =
            x_ / width * z

        moveY =
            y_ / height * z
    in
    { model
        | z = model.z * (1 + z)
        , x = model.x - width * model.z * moveX
        , y = model.y - height * model.z * moveY
    }


vector : Coords -> Coords -> Model -> Coords
vector a b { z } =
    { x = (b.x - a.x) * z
    , y = (b.y - a.y) * z
    }


updateByBoundingBox : Model -> { width : Float, height : Float } -> BBox -> Model
updateByBoundingBox model { width, height } bbox =
    { x = bbox.x + bbox.width / 2 - (2 * expandHandleWidth + entityWidth) / 2
    , y = bbox.y + bbox.height / 2 - (entityMinHeight + addressHeight) / 2
    , z = model.z
    }
