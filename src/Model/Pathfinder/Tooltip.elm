module Model.Pathfinder.Tooltip exposing (Tooltip, TooltipType(..))

import Api.Data exposing (Actor, TagSummary)
import Hovercard
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Tx as Tx


type alias Tooltip =
    { hovercard : Hovercard.Model
    , type_ : TooltipType
    }


type TooltipType
    = UtxoTx Tx.UtxoTx
    | AccountTx Tx.AccountTx
    | Address Address
    | TagLabel String TagSummary
    | ActorDetails Actor
