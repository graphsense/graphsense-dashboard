module Init.Graph.Transform exposing (..)

import Bounce
import Model.Graph.Transform exposing (..)
import Number.Bounded as Bounded
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
            , z =
                Bounded.between 0.1 14
                    |> Bounded.set 1
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
