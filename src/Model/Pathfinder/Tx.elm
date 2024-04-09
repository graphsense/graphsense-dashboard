module Model.Pathfinder.Tx exposing (..)

import Api.Data
import Dict exposing (Dict)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Input exposing (Input)
import Model.Pathfinder.Output exposing (Output)


type Tx
    = Account AccontTx
    | Utxo UtxoTx


type alias AccontTx =
    { from : String
    , to : String
    , value : Api.Data.Values
    , id : Id
    }


type alias UtxoTx =
    { inputs : Dict Id Input
    , outputs : Dict Id Output
    , id : Id
    }
