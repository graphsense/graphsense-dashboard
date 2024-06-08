module Init.Graph.Transform exposing (..)

import Bounce
import Model.Graph.Transform exposing (..)
import Number.Bounded as Bounded
import RecordSetter exposing (s_state)
import Set
import Number.Bounded exposing (Bounded)


init : Model id
init =
    { collectingAddedEntityIds = Set.empty
    , bounce = Bounce.init
    , state =
        Settled
            { x = 0
            , y = 0
            , z = initZ
            }
    }


initZ : Bounded Float
initZ =

                Bounded.between 0.1 14
                    |> Bounded.set 1


initTransitioning : Bool -> Float -> Coords -> Coords -> Model id
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
