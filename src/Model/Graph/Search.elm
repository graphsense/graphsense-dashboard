module Model.Graph.Search exposing (Criterion(..), Direction(..), Model)

import Api.Data
import Browser.Dom as Dom
import Hovercard
import Model.Graph.Id exposing (EntityId)


type alias Model =
    { direction : Direction
    , criterion : Criterion
    , id : EntityId
    , hovercard : Hovercard.Model
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
