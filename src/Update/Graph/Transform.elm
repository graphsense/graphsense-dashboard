module Update.Graph.Transform exposing (delay, move, pop, transition, update, updateByBoundingBox, vector, wheel)

import Basics.Extra exposing (flip)
import Bounce
import Ease
import Init.Graph.Transform exposing (initTransitioning)
import Model.Graph.Coords as Graph exposing (BBox)
import Model.Graph.Transform as Transform exposing (..)
import Msg.Graph as Graph
import Number.Bounded as Bounded
import RecordSetter exposing (..)
import Set exposing (Set)


update : Graph.Coords -> Graph.Coords -> Model comparable -> Model comparable
update start current transform =
    transform
        |> addX (start.x - current.x)
        |> addY (start.y - current.y)


addX : Float -> Model comparable -> Model comparable
addX =
    add .x s_x


addY : Float -> Model comparable -> Model comparable
addY =
    add .y s_y


add : (Transform.Coords -> Float) -> (Float -> Transform.Coords -> Transform.Coords) -> Float -> Model comparable -> Model comparable
add field upd delta model =
    case model.state of
        Transitioning t ->
            { model
                | state =
                    { t
                        | current =
                            t.current
                                |> upd (delta * Bounded.value t.current.z + field t.current)
                    }
                        |> Transitioning
            }

        Settled t ->
            { model
                | state =
                    t
                        |> upd (delta * Bounded.value t.z + field t)
                        |> Settled
            }


wheel : { width : Float, height : Float } -> Float -> Float -> Float -> Model comparable -> Model comparable
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

        upd co =
            let
                newZ =
                    Bounded.map ((*) (1 + z)) co.z

                changed =
                    Bounded.value newZ /= Bounded.value co.z
            in
            if changed then
                co
                    |> s_x (co.x - width * Bounded.value co.z * moveX)
                    |> s_y (co.y - height * Bounded.value co.z * moveY)
                    |> s_z newZ

            else
                co
    in
    case model.state of
        Transitioning t ->
            { model
                | state =
                    { t
                        | to = upd t.to
                        , from = t.current
                    }
                        |> Transitioning
            }

        Settled t ->
            upd t
                |> initTransitioning False 100 t


vector : Graph.Coords -> Graph.Coords -> Model comparable -> Graph.Coords
vector a b model =
    { x = (b.x - a.x) * getZ model
    , y = (b.y - a.y) * getZ model
    }


updateByBoundingBox : Model comparable -> BBox -> { width : Float, height : Float } -> Model comparable
updateByBoundingBox model bbox { width, height } =
    let
        current =
            getCurrent model

        coords =
            { x = bbox.x + bbox.width / 2
            , y = bbox.y + bbox.height / 2
            , z =
                max
                    (bbox.width / width)
                    (bbox.height / height)
                    |> max 1
                    |> flip Bounded.set current.z
            }
    in
    if Transform.equals coords current then
        model |> s_state (Settled coords)

    else
        move coords model


move : Coords -> Model comparable -> Model comparable
move coords model =
    case model.state of
        Transitioning t ->
            { model
                | state =
                    t
                        |> s_to coords
                        |> s_from t.current
                        |> s_progress 0
                        |> Transitioning
            }

        Settled t ->
            initTransitioning True defaultDuration t coords


transition : Float -> Model comparable -> Model comparable
transition delta model =
    { model
        | state =
            case model.state of
                Settled _ ->
                    model.state

                Transitioning t ->
                    let
                        progress =
                            t.progress + delta

                        ease =
                            if t.withEase then
                                Ease.outQuad

                            else
                                identity

                        prg =
                            progress
                                / t.duration
                                |> ease
                    in
                    if progress < t.duration then
                        { t
                            | progress = progress
                            , current =
                                { x = t.from.x + (t.to.x - t.from.x) * prg
                                , y = t.from.y + (t.to.y - t.from.y) * prg
                                , z =
                                    Bounded.inc
                                        ((Bounded.value t.to.z - Bounded.value t.from.z) * prg)
                                        t.from.z
                                }
                        }
                            |> Transitioning

                    else
                        Settled t.to
    }


delay : Set comparable -> Model comparable -> ( Model comparable, Cmd Graph.Msg )
delay ids model =
    ( { model
        | bounce = Bounce.push model.bounce
        , collectingAddedEntityIds = Set.union ids model.collectingAddedEntityIds
      }
    , Bounce.delay 100 Graph.RuntimeDebouncedAddingEntities
    )


pop : Model comparable -> ( Model comparable, Bool )
pop model =
    let
        newBounce =
            Bounce.pop model.bounce

        isSteady =
            Bounce.steady newBounce
    in
    ( { model
        | bounce = newBounce
        , collectingAddedEntityIds =
            if isSteady then
                Set.empty

            else
                model.collectingAddedEntityIds
      }
    , isSteady
    )
