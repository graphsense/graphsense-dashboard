module Effect.Graph exposing (Effect(..), perform)


type Effect
    = NoEffect


perform : Effect -> Cmd msg
perform eff =
    Cmd.none
