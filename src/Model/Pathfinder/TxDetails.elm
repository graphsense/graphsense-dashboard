module Model.Pathfinder.TxDetails exposing (Model)

import Api.Data
import Components.Table exposing (Table)
import Model.Pathfinder.Tx exposing (Tx)


type alias Model =
    { inputsTableOpen : Bool
    , outputsTableOpen : Bool
    , inputsTable : Table Api.Data.TxValue
    , outputsTable : Table Api.Data.TxValue
    , tx : Tx
    }
