module Model.Direction exposing (..)


type Direction
    = Incoming
    | Outgoing


toString : Direction -> String
toString dir =
    case dir of
        Incoming ->
            "incoming"

        Outgoing ->
            "outgoing"


signOffsetByDirection : Direction -> Float -> Float
signOffsetByDirection direction offset =
    case direction of
        Incoming ->
            -offset

        Outgoing ->
            offset
