module Model.Graph.Search exposing (..)

import Api.Data
import Browser.Dom as Dom
import Model.Graph.Id exposing (EntityId)


type alias Model =
    { direction : Direction
    , criterion : Criterion
    , id : EntityId
    , element : Dom.Element
    , depth : String
    , breadth : String
    , maxAddresses : String
    }


type Criterion
    = Category (List Api.Data.Concept) String


type Direction
    = Incoming
    | Outgoing
    | Both
