module Update.Graph.Transform exposing (dragEnd, dragStart, get, mousemove, update, wheel)

import List.Extra
import Model.Graph.Transform exposing (..)
import Pixels
import Point2d
import RecordSetter exposing (..)
import Rectangle2d


dragStart : Coords -> Model -> Model
dragStart coords model =
    { model
        | dragging =
            case model.dragging of
                NoDragging ->
                    Dragging coords coords

                Dragging _ _ ->
                    model.dragging
    }


mousemove : Coords -> Model -> Model
mousemove coords model =
    { model
        | dragging =
            case model.dragging of
                NoDragging ->
                    model.dragging

                Dragging start _ ->
                    Dragging start coords
        , mouse = coords
    }


dragEnd : Model -> Model
dragEnd model =
    case model.dragging of
        NoDragging ->
            model

        Dragging start current ->
            { model
                | dragging =
                    NoDragging
                , transform =
                    update start current model.transform
            }


update : Coords -> Coords -> { x : Float, y : Float, z : Float } -> { x : Float, y : Float, z : Float }
update start current transform =
    transform
        |> s_x ((start.x - current.x) * transform.z + transform.x)
        |> s_y ((start.y - current.y) * transform.z + transform.y)


wheel : { width : Float, height : Float } -> Float -> Float -> Float -> Model -> Model
wheel { width, height } _ y _ model =
    if model.dragging /= NoDragging then
        model

    else
        let
            factor =
                0.01

            z =
                y
                    * factor
                    |> max -0.9

            moveX =
                model.mouse.x / width * z

            moveY =
                model.mouse.y / height * z
        in
        { model
            | transform =
                model.transform
                    |> s_z (model.transform.z * (1 + z))
                    |> s_x (model.transform.x - width * model.transform.z * moveX)
                    |> s_y (model.transform.y - height * model.transform.z * moveY)
        }


get : Model -> { x : Float, y : Float, z : Float }
get model =
    case model.dragging of
        NoDragging ->
            model.transform

        Dragging start current ->
            update start current model.transform
