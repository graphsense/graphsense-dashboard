module Update.Graph.Transform exposing (drag, dragEnd, dragStart, get)

import Model.Graph.Transform exposing (..)
import RecordSetter exposing (..)


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


drag : Coords -> Model -> Model
drag coords model =
    { model
        | dragging =
            case model.dragging of
                NoDragging ->
                    model.dragging

                Dragging start _ ->
                    Dragging start coords
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
        |> s_x (start.x - current.x + transform.x)
        |> s_y (start.y - current.y + transform.y)


get : Model -> { x : Float, y : Float, z : Float }
get model =
    case model.dragging of
        NoDragging ->
            model.transform

        Dragging start current ->
            update start current model.transform
