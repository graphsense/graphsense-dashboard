module Model.Pathfinder.Tooltip exposing (..)

import Hovercard
import Model.Pathfinder.Tx as Tx


type alias Tooltip =
    { hovercard : Hovercard.Model
    , type_ : TooltipType
    }


type TooltipType
    = UtxoTx Tx.UtxoTx
