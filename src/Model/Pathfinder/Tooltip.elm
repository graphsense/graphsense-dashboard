module Model.Pathfinder.Tooltip exposing (Tooltip, TooltipMessages, TooltipType(..), isSameTooltip, mapMsgTooltipMsg, mapMsgTooltipType)

import Api.Data exposing (Actor, TagSummary)
import Hovercard
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx


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
    = UtxoTx Tx.UtxoTx
    | AccountTx Tx.AccountTx
    | AggEdge { leftAddress : Id, left : Maybe Api.Data.NeighborAddress, rightAddress : Id, right : Maybe Api.Data.NeighborAddress }
    | Address Address (Maybe TagSummary)
    | TagLabel String TagSummary (TooltipMessages msg)
    | TagConcept Id String TagSummary (TooltipMessages msg)
    | ActorDetails Actor (TooltipMessages msg)
    | Text String
    | Plugin { context : String, domId : String } (TooltipMessages msg)


mapMsgTooltipMsg : TooltipMessages msgA -> (msgA -> msgB) -> TooltipMessages msgB
mapMsgTooltipMsg m f =
    { openTooltip = f m.openTooltip, closeTooltip = f m.closeTooltip, openDetails = m.openDetails |> Maybe.map f }


mapMsgTooltipType : TooltipType msgA -> (msgA -> msgB) -> TooltipType msgB
mapMsgTooltipType toMap f =
    case toMap of
        TagLabel a b msgs ->
            TagLabel a b (mapMsgTooltipMsg msgs f)

        TagConcept a b c msgs ->
            TagConcept a b c (mapMsgTooltipMsg msgs f)

        ActorDetails a msgs ->
            ActorDetails a (mapMsgTooltipMsg msgs f)

        Address a b ->
            Address a b

        AccountTx a ->
            AccountTx a

        UtxoTx a ->
            UtxoTx a

        AggEdge a ->
            AggEdge a

        Text a ->
            Text a

        Plugin pid msgs ->
            Plugin pid (mapMsgTooltipMsg msgs f)


isSameTooltip : Tooltip msg -> Tooltip msg -> Bool
isSameTooltip t1 t2 =
    case ( t1.type_, t2.type_ ) of
        ( UtxoTx tx1, UtxoTx tx2 ) ->
            tx1 == tx2

        ( AccountTx tx1, AccountTx tx2 ) ->
            tx1 == tx2

        ( Address a1 _, Address a2 _ ) ->
            a1.id == a2.id

        ( TagLabel id1 _ _, TagLabel id2 _ _ ) ->
            id1 == id2

        ( TagConcept a1 id1 _ _, TagConcept a2 id2 _ _ ) ->
            id1 == id2 && a1 == a2

        ( ActorDetails a1 _, ActorDetails a2 _ ) ->
            a1.id == a2.id

        ( Text tt1, Text tt2 ) ->
            tt1 == tt2

        ( AggEdge tt1, AggEdge tt2 ) ->
            tt1.leftAddress
                == tt2.leftAddress
                && tt1.rightAddress
                == tt2.rightAddress

        ( Plugin p1 _, Plugin p2 _ ) ->
            p1.domId == p2.domId

        ( UtxoTx _, _ ) ->
            False

        ( AccountTx _, _ ) ->
            False

        ( Address _ _, _ ) ->
            False

        ( TagLabel _ _ _, _ ) ->
            False

        ( TagConcept _ _ _ _, _ ) ->
            False

        ( ActorDetails _ _, _ ) ->
            False

        ( Text _, _ ) ->
            False

        ( AggEdge _, _ ) ->
            False

        ( Plugin _ _, _ ) ->
            False
