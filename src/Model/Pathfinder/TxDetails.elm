module Model.Pathfinder.TxDetails exposing (..)

import Api.Data
import Model.Graph.Table exposing (Table)
import Model.Pathfinder.Tx exposing (Tx)


type alias Model =
    { inputsTableOpen : Bool
    , outputsTableOpen : Bool
    , inputsTable : Table Api.Data.TxValue
    , outputsTable : Table Api.Data.TxValue
    , tx : Tx
    }
