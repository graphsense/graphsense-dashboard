module Model.Pathfinder.Tooltip exposing (Tooltip, TooltipMessages, TooltipType(..), isSameTooltip, mapMsgTooltipMsg, mapMsgTooltipType)

import Api.Data exposing (Actor, TagSummary)
import Hovercard
import Model.Pathfinder.Id exposing (Id)


type alias Tooltip msg =
    { hovercard : Hovercard.Model
    , type_ : TooltipType msg
    , closing : Bool
    , open : Bool
    }


type alias TooltipMessages msg =
    { openTooltip : msg
    , closeTooltip : msg
    , openDetails : Maybe msg
    }


type TooltipType msg
    = ActorDetails Actor (TooltipMessages msg)
    | Plugin { context : String, domId : String } (TooltipMessages msg)


mapMsgTooltipMsg : TooltipMessages msgA -> (msgA -> msgB) -> TooltipMessages msgB
mapMsgTooltipMsg m f =
    { openTooltip = f m.openTooltip, closeTooltip = f m.closeTooltip, openDetails = m.openDetails |> Maybe.map f }


mapMsgTooltipType : TooltipType msgA -> (msgA -> msgB) -> TooltipType msgB
mapMsgTooltipType toMap f =
    case toMap of
        ActorDetails a msgs ->
            ActorDetails a (mapMsgTooltipMsg msgs f)

        Plugin pid msgs ->
            Plugin pid (mapMsgTooltipMsg msgs f)


isSameTooltip : Tooltip msg -> Tooltip msg -> Bool
isSameTooltip t1 t2 =
    case ( t1.type_, t2.type_ ) of
        ( ActorDetails a1 _, ActorDetails a2 _ ) ->
            a1.id == a2.id

        ( Plugin p1 _, Plugin p2 _ ) ->
            p1.domId == p2.domId

        ( ActorDetails _ _, _ ) ->
            False

        ( Plugin _ _, _ ) ->
            False
