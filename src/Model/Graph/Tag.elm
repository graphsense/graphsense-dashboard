module Model.Graph.Tag exposing (..)

import Api.Data
import Browser.Dom as Dom
import Model.Graph.Id exposing (AddressId)
import Model.Search as Search


type alias Model =
    { input : Input
    , hovercardElement : Dom.Element
    }


type alias UserTag =
    { label : String
    , source : String
    , category : Maybe String
    , abuse : Maybe String
    }


type alias Input =
    { label : Search.Model
    , source : String
    , category : String
    , abuse : String
    , id : AddressId
    }


userTagToApiTag : Api.Data.Address -> Bool -> UserTag -> Api.Data.AddressTag
userTagToApiTag { currency, address } isClusterDefiner tag =
    { abuse = tag.abuse
    , address = address
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
    }
