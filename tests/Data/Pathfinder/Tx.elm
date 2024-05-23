module Data.Pathfinder.Tx exposing (..)

import Config.Pathfinder exposing (nodeXOffset)
import Data.Api as Api
import Data.Pathfinder.Id as Id
import Dict.Nonempty as NDict
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tx as Tx


tx1 : Tx.Tx
tx1 =
    { id = Id.tx1
    , type_ =
        Tx.Utxo
            { x = nodeXOffset
            , y = 0
            , inputs = NDict.singleton Id.address1 Api.values
            , outputs =
                NDict.singleton Id.address3 Api.values
                    |> NDict.insert Id.address4 Api.values
                    |> NDict.insert Id.address5 Api.values
            }
    , raw = Api.tx1
    }
