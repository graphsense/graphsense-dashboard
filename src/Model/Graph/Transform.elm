module Model.Graph.Transform exposing (..)

import Bounce exposing (Bounce)
import Model.Graph.Coords exposing (BBox)
import Model.Graph.Id as Id
import Number.Bounded as Bounded exposing (Bounded)
import RecordSetter exposing (..)
import Set exposing (Set)


defaultDuration : Float
defaultDuration =
    500


type alias Coords =
    { x : Float
    , y : Float
    , z : Bounded Float
    }


type alias Model =
    { state : Transition
    , collectingAddedEntityIds : Set Id.EntityId
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


getZ : Model -> Float
getZ model =
    case model.state of
        Transitioning { current } ->
            Bounded.value current.z

        Settled { z } ->
            Bounded.value z


getCurrent : Model -> Coords
getCurrent model =
    case model.state of
        Transitioning { current } ->
            current

        Settled c ->
            c


getBoundingBox : Model -> { width : Float, height : Float } -> BBox
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
