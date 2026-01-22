module Model.Pathfinder.Deserialize exposing (Deserialized, DeserializedAggEdge, DeserializedAnnotation, DeserializedThing, Deserializing)

import Api.Data
import Color exposing (Color)
import Model.Pathfinder.Id exposing (Id)
import Set exposing (Set)


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
    , aggEdges : List DeserializedAggEdge
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
    , index : Int
    }


type alias DeserializedAggEdge =
    { a : Id
    , b : Id
    , txs : Set Id
    }
