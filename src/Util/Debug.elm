module Util.Debug exposing (addDebugToUpdate)


addDebugToUpdate : (msg -> model -> ( model, effect )) -> (msg -> model -> ( model, effect ))
addDebugToUpdate update =
    \msg model ->
        update (Debug.log "Incoming Message" msg) (Debug.log "Incoming Model" model)
            |> Tuple.mapFirst (Debug.log "Outgoing Model")
            |> Tuple.mapSecond (Debug.log "Outgoing Effect")
