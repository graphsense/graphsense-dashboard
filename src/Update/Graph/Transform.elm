module Update.Graph.Transform exposing (update, vector, wheel)

import List.Extra
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Transform exposing (..)
import Pixels
import Point2d
import RecordSetter exposing (..)
import Rectangle2d


update : Coords -> Coords -> Model -> Model
update start current transform =
    transform
        |> s_x ((start.x - current.x) * transform.z + transform.x)
        |> s_y ((start.y - current.y) * transform.z + transform.y)


wheel : { width : Float, height : Float, mouse : Coords } -> Float -> Float -> Float -> Model -> Model
wheel { width, height, mouse } _ y _ model =
    let
        factor =
            0.01

        z =
            y
                * factor
                |> max -0.9

        moveX =
            mouse.x / width * z

        moveY =
            mouse.y / height * z
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
