module Model.Pathfinder.TxDetails exposing (..)

import Model.Pathfinder.Tx exposing (Tx)


type alias Model =
    { ioTableOpen : Bool
    , tx : Tx
    }
