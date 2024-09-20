module Model.Pathfinder.Deserialize exposing (Deserialized, DeserializedThing, Deserializing)

import Api.Data
import Model.Pathfinder.Id exposing (Id)


type alias Deserializing =
    { deserialized : Deserialized
    , addresses : List Api.Data.Address
    , txs : List Api.Data.Tx
    }


type alias Deserialized =
    { addresses : List DeserializedThing
    , txs : List DeserializedThing
    }


type alias DeserializedThing =
    { id : Id
    , x : Float
    , y : Float
    , isStartingPoint : Bool
    }
