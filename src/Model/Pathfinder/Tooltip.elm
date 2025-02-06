module Model.Pathfinder.Tooltip exposing (Tooltip, TooltipMessages, TooltipType(..), isSameTooltip)

import Api.Data exposing (Actor, TagSummary)
import Hovercard
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx


type alias Tooltip msg =
    { hovercard : Hovercard.Model
    , type_ : TooltipType msg
    , closing : Bool
    }


type alias TooltipMessages msg =
    { openTooltip : msg
    , closeTooltip : msg
    , openDetails : Maybe msg
    }


type TooltipType msg
    = UtxoTx Tx.UtxoTx
    | AccountTx Tx.AccountTx
    | Address Address (Maybe TagSummary)
    | TagLabel String TagSummary (TooltipMessages msg)
    | TagConcept Id String TagSummary (TooltipMessages msg)
    | ActorDetails Actor (TooltipMessages msg)
    | Text String


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
            t1 == t2

        _ ->
            False
