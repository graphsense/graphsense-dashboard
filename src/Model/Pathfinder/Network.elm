module Model.Pathfinder.Network exposing (..)

import Dict exposing (Dict)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (Tx)


type alias Network =
    { name : String
    , addresses : Dict Id Address
    , txs : Dict Id Tx
    }
