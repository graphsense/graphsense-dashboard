module Model.Direction exposing (Direction(..), flip, signOffsetByDirection, toString)


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


flip : Direction -> Direction
flip dir =
    case dir of
        Incoming ->
            Outgoing

        Outgoing ->
            Incoming
