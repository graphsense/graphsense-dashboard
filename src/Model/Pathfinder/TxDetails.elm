module Model.Pathfinder.TxDetails exposing (..)

import Api.Data
import Model.Pathfinder.Tx exposing (Tx)
import Model.Graph.Table exposing (Table)


type alias Model =
    { ioTableOpen : Bool
    , table : Table Api.Data.TxValue
    , tx : Tx
    }
