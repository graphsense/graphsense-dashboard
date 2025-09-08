module Model.Graph.Transform exposing (Coords, Model, Transition(..), coordsToBBox, defaultDuration, equals, getBoundingBox, getCurrent, getZ)

import Bounce exposing (Bounce)
import Model.Graph.Coords exposing (BBox)
import Number.Bounded as Bounded exposing (Bounded)
import Set exposing (Set)


defaultDuration : Float
defaultDuration =
    500


type alias Coords =
    { x : Float
    , y : Float
    , z : Bounded Float
    }


type alias Model comparable =
    { state : Transition
    , collectingAddedEntityIds : Set comparable
    , bounce : Bounce
    }


type Transition
    = Transitioning
        { from : Coords
        , to : Coords
        , current : Coords
        , progress : Float
        , duration : Float
        , withEase : Bool
        }
    | Settled Coords


getZ : Model comparable -> Float
getZ model =
    case model.state of
        Transitioning { current } ->
            Bounded.value current.z

        Settled { z } ->
            Bounded.value z


getCurrent : Model comparable -> Coords
getCurrent model =
    case model.state of
        Transitioning { current } ->
            current

        Settled c ->
            c


getBoundingBox : Model comparable -> { width : Float, height : Float } -> BBox
getBoundingBox model { width, height } =
    let
        current =
            getCurrent model

        bbwidth =
            current.z
                |> Bounded.value
                |> (*) width

        bbheight =
            current.z
                |> Bounded.value
                |> (*) height
    in
    { x = current.x - bbwidth / 2
    , y = current.y - bbheight / 2
    , width = bbwidth
    , height = bbheight
    }


equals : Coords -> Coords -> Bool
equals a b =
    a.x
        == b.x
        && a.y
        == b.y
        && Bounded.value a.z
        == Bounded.value b.z


coordsToBBox : { width : Float, height : Float } -> Coords -> BBox
coordsToBBox { width, height } coords =
    let
        z =
            Bounded.value coords.z
    in
    { x = coords.x - width / 2 * z
    , y = coords.y - height / 2 * z
    , width = max 0 <| width * z
    , height = max 0 <| height * z
    }
