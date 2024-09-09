module Generate.Common.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Elm
import Generate.Util exposing (..)
import RecordSetter exposing (..)
import Types exposing (Details, OriginAdjust)


adjustBoundingBox : OriginAdjust -> { a | defaultShapeTraits : DefaultShapeTraits } -> { a | defaultShapeTraits : DefaultShapeTraits }
adjustBoundingBox { x, y } node =
    node.defaultShapeTraits.absoluteBoundingBox
        |> (\bb -> { bb | x = bb.x - x, y = bb.y - y })
        |> flip s_absoluteBoundingBox node.defaultShapeTraits
        |> flip s_defaultShapeTraits node


adjustName : Dict String String -> { a | defaultShapeTraits : DefaultShapeTraits } -> { a | defaultShapeTraits : DefaultShapeTraits }
adjustName names node =
    Dict.get (getId node) names
        |> Maybe.map
            (flip s_name node.defaultShapeTraits.isLayerTrait
                >> flip s_isLayerTrait node.defaultShapeTraits
                >> flip s_defaultShapeTraits node
            )
        |> Maybe.withDefault node


getId : { a | defaultShapeTraits : DefaultShapeTraits } -> String
getId node =
    node.defaultShapeTraits.isLayerTrait.id


getName : { a | defaultShapeTraits : DefaultShapeTraits } -> String
getName node =
    node.defaultShapeTraits.isLayerTrait.name


getNameId : { a | defaultShapeTraits : DefaultShapeTraits } -> ( String, String )
getNameId node =
    ( getName node
    , getId node
    )


getBoundingBox : { a | defaultShapeTraits : DefaultShapeTraits } -> Rectangle
getBoundingBox node =
    node.defaultShapeTraits.absoluteBoundingBox


getStrokeWidth : { a | defaultShapeTraits : DefaultShapeTraits } -> Float
getStrokeWidth node =
    node.defaultShapeTraits.strokeWeight
        |> Maybe.withDefault 0


toDetails : List Elm.Expression -> { a | defaultShapeTraits : DefaultShapeTraits } -> Details
toDetails styles node =
    { name = getName node
    , instanceName = ""
    , bbox = getBoundingBox node
    , strokeWidth = getStrokeWidth node
    , styles = styles
    }
