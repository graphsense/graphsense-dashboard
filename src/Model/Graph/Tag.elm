module Model.Graph.Tag exposing (..)

import Api.Data
import Browser.Dom as Dom
import Hovercard
import Model.Graph.Id exposing (AddressId, EntityId)
import Model.Node exposing (Node)
import Model.Search as Search


type alias Model =
    { input : Input
    , existing : Maybe UserTag
    , hovercard : Hovercard.Model
    }


type alias UserTag =
    { currency : String
    , address : String
    , label : String
    , source : String
    , category : Maybe String
    , abuse : Maybe String
    , isClusterDefiner : Bool
    }


type alias Input =
    { label : Search.Model
    , source : String
    , category : String
    , abuse : String
    , id : Node AddressId EntityId
    }


userTagToApiTag : { currency : String, entity : Int, address : String } -> Bool -> UserTag -> Api.Data.AddressTag
userTagToApiTag { currency, address, entity } isClusterDefiner tag =
    { abuse = tag.abuse
    , address = address
    , entity = entity
    , category = tag.category
    , confidence = Nothing
    , confidenceLevel = Nothing
    , currency = currency
    , isClusterDefiner = isClusterDefiner
    , label = tag.label
    , lastmod = Nothing
    , source = Just tag.source
    , tagpackCreator = ""
    , tagpackIsPublic = False
    , tagpackTitle = ""
    , tagpackUri = Nothing
    , actor = Nothing
    }
