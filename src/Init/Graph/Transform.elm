module Init.Graph.Transform exposing (..)

import Bounce
import Model.Graph.Transform exposing (..)
import RecordSetter exposing (s_state)
import Set


init : Model
init =
    { collectingAddedEntityIds = Set.empty
    , bounce = Bounce.init
    , state =
        Settled
            { x = 0
            , y = 0
            , z = 1
            }
    }


initTransitioning : Bool -> Float -> Coords -> Coords -> Model
initTransitioning withEase duration from to =
    init
        |> s_state
            (Transitioning
                { from = from
                , to = to
                , current = from
                , progress = 0
                , duration = duration
                , withEase = withEase
                }
            )
