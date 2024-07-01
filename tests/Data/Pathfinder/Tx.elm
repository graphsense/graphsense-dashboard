module Data.Pathfinder.Tx exposing (..)

import Animation
import Config.Pathfinder exposing (nodeXOffset, nodeYOffset)
import Data.Api as Api
import Data.Pathfinder.Id as Id
import Dict.Nonempty as NDict
import Model.Pathfinder.Id as Id
import Model.Pathfinder.Tx as Tx exposing (Io)


tx1 : Tx.Tx
tx1 =
    { id = Id.tx1
    , type_ =
        Tx.Utxo
            { x = nodeXOffset
            , y = Animation.static 0
            , dx = 0
            , dy = 0
            , selected = False
            , opacity = Animation.static 0
            , clock = 0
            , inputs = NDict.singleton Id.address1 (Io Api.values False)
            , outputs =
                NDict.singleton Id.address3 (Io Api.values False)
                    |> NDict.insert Id.address4 (Io Api.values False)
                    |> NDict.insert Id.address5 (Io Api.values False)
            , raw = Api.tx1
            }
    }


tx2 : Tx.Tx
tx2 =
    { id = Id.tx2
    , type_ =
        Tx.Utxo
            { x = -nodeXOffset
            , y = Animation.static 0
            , dx = 0
            , dy = 0
            , selected = False
            , opacity = Animation.static 0
            , clock = 0
            , outputs = NDict.singleton Id.address1 <| Io Api.values False
            , inputs = NDict.singleton Id.address6 <| Io Api.values False
            , raw = Api.tx2
            }
    }


tx3 : Tx.Tx
tx3 =
    { id = Id.tx3
    , type_ =
        Tx.Utxo
            { x = nodeXOffset
            , y = Animation.static <| 3 * nodeYOffset
            , dx = 0
            , dy = 0
            , selected = False
            , opacity = Animation.static 0
            , clock = 0
            , outputs = NDict.singleton Id.address7 <| Io Api.values False
            , inputs = NDict.singleton Id.address1 <| Io Api.values False
            , raw = Api.tx3
            }
    }
