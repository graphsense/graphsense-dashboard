module Init.Graph.Search exposing (init, initCriterion)

import Api.Data
import Hovercard
import Model.Graph.Id exposing (EntityId, entityIdToString)
import Model.Graph.Search exposing (..)
import Msg.Graph exposing (Msg(..))
import Tuple exposing (mapSecond)


init : List Api.Data.Concept -> EntityId -> ( Model, Cmd Msg )
init categories entityId =
    let
        ( hovercard, cmd ) =
            entityIdToString entityId
                |> Hovercard.init
                |> mapSecond (Cmd.map SearchHovercardMsg)
    in
    ( { direction = Outgoing
      , criterion = initCriterion categories
      , id = entityId
      , hovercard = hovercard
      , depth = "2"
      , breadth = "20"
      , maxAddresses = "100"
      }
    , cmd
    )


initCriterion : List Api.Data.Concept -> Criterion
initCriterion categories =
    Category categories "exchange"
