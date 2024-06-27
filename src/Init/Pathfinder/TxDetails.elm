module Init.Pathfinder.TxDetails exposing (..)

import Model.Pathfinder.Tx exposing (Tx)
import Model.Pathfinder.TxDetails as TxDetails


init : Tx -> TxDetails.Model
init tx =
    { ioTableOpen = False
    , tx = tx
    }
