module Model.Pathfinder.TxDetails exposing (..)

import Api.Data
import Model.Graph.Table exposing (Table)
import Model.Pathfinder.Tx exposing (Tx)


type alias Model =
    { ioTableOpen : Bool
    , table : Table Api.Data.TxValue
    , tx : Tx
    }
