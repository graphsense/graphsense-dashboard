module Init.Pathfinder.Relation exposing (init, initRelation)

import Dict
import IntDict
import Model.Pathfinder.Relation exposing (Relation, RelationType, Relations)


init : Relations
init =
    { relations = IntDict.empty
    , txRelationMap = Dict.empty
    , aggEdgeRelationMap = Dict.empty
    , nextInt = 0
    }


initRelation : Int -> RelationType -> Model.Pathfinder.Relation.Relation
initRelation =
    Relation
