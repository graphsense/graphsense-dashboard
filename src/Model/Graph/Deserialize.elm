module Model.Graph.Deserialize exposing (Deserialized, DeserializedAddress, DeserializedEntity, DeserializedEntityTag(..), DeserializedEntityUserTag, Deserializing)

import Api.Data
import Color exposing (Color)
import Model.Graph.Id exposing (AddressId, EntityId)
import Model.Graph.Tag as Tag


type alias Deserializing =
    { deserialized : Deserialized
    , addresses : List Api.Data.Address
    , entities : List Api.Data.Entity
    }


type alias Deserialized =
    { addresses : List DeserializedAddress
    , entities : List DeserializedEntity
    , highlights : List ( String, Color )
    }


type alias DeserializedAddress =
    { id : AddressId
    , x : Float
    , y : Float
    , userTag : Maybe Tag.UserTag
    , color : Maybe Color
    }


type DeserializedEntityTag
    = TagUserTag Tag.UserTag
    | DeserializedEntityUserTagTag DeserializedEntityUserTag


type alias DeserializedEntity =
    { id : EntityId
    , rootAddress : Maybe String
    , x : Float
    , y : Float
    , color : Maybe Color
    , userTag : Maybe DeserializedEntityTag
    , noAddresses : Int
    }


type alias DeserializedEntityUserTag =
    { currency : String
    , entity : Int
    , label : String
    , source : String
    , category : Maybe String
    , abuse : Maybe String
    }
