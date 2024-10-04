module Model.Pathfinder.Deserialize exposing (Deserialized, DeserializedAnnotation, DeserializedThing, Deserializing)

import Api.Data
import Color exposing (Color)
import Model.Pathfinder.Id exposing (Id)


type alias Deserializing =
    { deserialized : Deserialized
    , addresses : List Api.Data.Address
    , txs : List Api.Data.Tx
    }


type alias Deserialized =
    { name : String
    , addresses : List DeserializedThing
    , txs : List DeserializedThing
    , annotations : List DeserializedAnnotation
    }


type alias DeserializedAnnotation =
    { id : Id
    , label : String
    , color : Maybe Color
    }


type alias DeserializedThing =
    { id : Id
    , x : Float
    , y : Float
    , isStartingPoint : Bool
    }
