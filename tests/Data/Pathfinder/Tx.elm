module Data.Pathfinder.Tx exposing (..)

import Animation
import Config.Pathfinder exposing (nodeXOffset, nodeYOffset)
import Data.Api as Api
import Data.Pathfinder.Id as Id
import Dict
import Model.Pathfinder.Tx as Tx exposing (Io)


tx1 : Tx.Tx
tx1 =
    { id = Id.tx1
    , x = nodeXOffset
    , y = Animation.static 0
    , dx = 0
    , dy = 0
    , clock = 0
    , opacity = Animation.static 0
    , selected = False
    , isStartingPoint = False
    , hovered = False
    , type_ =
        Tx.Utxo
            { inputs = Dict.singleton Id.address1 (Io Api.values Nothing 1)
            , outputs =
                Dict.singleton Id.address3 (Io Api.values Nothing 1)
                    |> Dict.insert Id.address4 (Io Api.values Nothing 1)
                    |> Dict.insert Id.address5 (Io Api.values Nothing 1)
            , raw = Api.tx1
            }
    }


tx2 : Tx.Tx
tx2 =
    { id = Id.tx2
    , x = -nodeXOffset
    , y = Animation.static 0
    , dx = 0
    , dy = 0
    , selected = False
    , opacity = Animation.static 0
    , clock = 0
    , isStartingPoint = False
    , hovered = False
    , type_ =
        Tx.Utxo
            { outputs = Dict.singleton Id.address1 <| Io Api.values Nothing 1
            , inputs = Dict.singleton Id.address6 <| Io Api.values Nothing 1
            , raw = Api.tx2
            }
    }


tx3 : Tx.Tx
tx3 =
    { id = Id.tx3
    , x = nodeXOffset
    , y = Animation.static <| 3 * nodeYOffset
    , dx = 0
    , dy = 0
    , selected = False
    , opacity = Animation.static 0
    , clock = 0
    , isStartingPoint = False
    , hovered = False
    , type_ =
        Tx.Utxo
            { outputs = Dict.singleton Id.address7 <| Io Api.values Nothing 1
            , inputs = Dict.singleton Id.address1 <| Io Api.values Nothing 1
            , raw = Api.tx3
            }
    }
