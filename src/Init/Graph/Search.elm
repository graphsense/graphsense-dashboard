module Init.Graph.Search exposing (..)

import Api.Data
import Browser.Dom as Dom
import Model.Graph.Id exposing (EntityId)
import Model.Graph.Search exposing (..)


init : List Api.Data.Concept -> Dom.Element -> EntityId -> Model
init categories element entityId =
    { direction = Outgoing
    , criterion = initCriterion categories
    , id = entityId
    , element = element
    , depth = 2
    , breadth = 20
    , maxAddresses = 100
    }


initCriterion : List Api.Data.Concept -> Criterion
initCriterion categories =
    Category categories "exchange"
