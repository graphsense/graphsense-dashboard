module Generate.Common.DefaultShapeTraits exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import RecordSetter exposing (..)
import Types exposing (Config, Details, OriginAdjust)


adjustBoundingBox : OriginAdjust -> DefaultShapeTraits -> DefaultShapeTraits
adjustBoundingBox { x, y } node =
    node.absoluteBoundingBox
        |> (\bb -> { bb | x = bb.x - x, y = bb.y - y })
        |> flip s_absoluteBoundingBox node


adjustName : Dict String String -> DefaultShapeTraits -> DefaultShapeTraits
adjustName names node =
    Dict.get (getId node) names
        |> Maybe.map
            (flip s_name node.isLayerTrait
                >> flip s_isLayerTrait node
            )
        |> Maybe.withDefault node


getId : DefaultShapeTraits -> String
getId defaultShapeTraits =
    defaultShapeTraits.isLayerTrait.id


getName : DefaultShapeTraits -> String
getName defaultShapeTraits =
    defaultShapeTraits.isLayerTrait.name


getNameId : DefaultShapeTraits -> ( String, String )
getNameId defaultShapeTraits =
    ( getName defaultShapeTraits
    , getId defaultShapeTraits
    )


getBoundingBox : DefaultShapeTraits -> Rectangle
getBoundingBox defaultShapeTraits =
    defaultShapeTraits.absoluteBoundingBox


getStrokeWidth : DefaultShapeTraits -> Float
getStrokeWidth defaultShapeTraits =
    defaultShapeTraits.strokeWeight
        |> Maybe.withDefault 0


toDetails : DefaultShapeTraits -> Details
toDetails defaultShapeTraits =
    let
        bbox =
            getBoundingBox defaultShapeTraits

        rbox =
            defaultShapeTraits.absoluteRenderBounds
                |> Maybe.withDefault bbox
    in
    { name = getName defaultShapeTraits
    , instanceName = ""
    , bbox = bbox
    , renderedSize =
        { width = rbox.width
        , height = rbox.height
        }
    , strokeWidth = getStrokeWidth defaultShapeTraits
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
