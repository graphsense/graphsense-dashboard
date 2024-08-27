module Model.Pathfinder.Tooltip exposing (..)

import Api.Data exposing (TagSummary)
import Hovercard
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Tx as Tx


type alias Tooltip =
    { hovercard : Hovercard.Model
    , type_ : TooltipType
    }


type TooltipType
    = UtxoTx Tx.UtxoTx
    | Address Address.Address
    | TagLabel String TagSummary
