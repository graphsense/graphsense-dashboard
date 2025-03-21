module Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType(..), isSameTooltip)

import Api.Data exposing (Actor, TagSummary)
import Hovercard
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx


type alias Tooltip =
    { hovercard : Hovercard.Model
    , type_ : TooltipType
    , closing : Bool
    }


type TooltipType
    = UtxoTx Tx.UtxoTx
    | AccountTx Tx.AccountTx
    | Address Address
    | TagLabel String TagSummary
    | TagConcept Id String TagSummary
    | ActorDetails Actor
    | Text String


isSameTooltip : Tooltip -> Tooltip -> Bool
isSameTooltip t1 t2 =
    case ( t1.type_, t2.type_ ) of
        ( UtxoTx tx1, UtxoTx tx2 ) ->
            tx1 == tx2

        ( AccountTx tx1, AccountTx tx2 ) ->
            tx1 == tx2

        ( Address a1, Address a2 ) ->
            a1.id == a2.id

        ( TagLabel id1 _, TagLabel id2 _ ) ->
            id1 == id2

        ( TagConcept a1 id1 _, TagConcept a2 id2 _ ) ->
            id1 == id2 && a1 == a2

        ( ActorDetails a1, ActorDetails a2 ) ->
            a1.id == a2.id

        ( Text tt1, Text tt2 ) ->
            t1 == t2

        _ ->
            False
