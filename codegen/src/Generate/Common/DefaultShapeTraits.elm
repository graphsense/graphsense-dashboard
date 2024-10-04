module Generate.Common.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import RecordSetter exposing (..)
import Types exposing (Config, Details, OriginAdjust)


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


isHidden : { a | defaultShapeTraits : DefaultShapeTraits } -> Bool
isHidden { defaultShapeTraits } =
    Maybe.map not defaultShapeTraits.isLayerTrait.visible
        |> Maybe.withDefault False


positionRelatively : Config -> { a | absoluteBoundingBox : Rectangle } -> List Elm.Expression
positionRelatively config node =
    case config.positionRelatively of
        Just { x, y } ->
            [ Css.position Css.absolute
            , Css.top <| Css.px (node.absoluteBoundingBox.y - y)
            , Css.left <| Css.px (node.absoluteBoundingBox.x - x)
            ]

        Nothing ->
            []
