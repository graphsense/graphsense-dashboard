module Generate.Common.FrameTraits exposing (..)

import Regex
import Api.Raw exposing (FrameTraits, Rectangle)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Elm
import RecordSetter exposing (..)
import Types exposing (Details, OriginAdjust)


adjustBoundingBox : OriginAdjust -> { a | frameTraits : FrameTraits } -> { a | frameTraits : FrameTraits }
adjustBoundingBox { x, y } node =
    let
        adjust bb =
            { bb | x = bb.x - x, y = bb.y - y }
    in
    node.frameTraits.absoluteBoundingBox
        |> adjust
        |> flip s_absoluteBoundingBox node.frameTraits
        |> (\frm ->
                frm.hasLayoutTrait.absoluteBoundingBox
                    |> Maybe.map adjust
                    |> flip s_absoluteBoundingBox frm.hasLayoutTrait
                    |> flip s_hasLayoutTrait frm
           )
        |> flip s_frameTraits node


adjustName : Dict String String -> { a | frameTraits : FrameTraits } -> { a | frameTraits : FrameTraits }
adjustName names node =
    Dict.get (getId node) names
        |> Maybe.map
            (flip s_name node.frameTraits.isLayerTrait
                >> flip s_isLayerTrait node.frameTraits
                >> flip s_frameTraits node
            )
        |> Maybe.withDefault node


getName : { a | frameTraits : FrameTraits } -> String
getName node =
    node.frameTraits.isLayerTrait.name


getId : { a | frameTraits : FrameTraits } -> String
getId node =
    node.frameTraits.isLayerTrait.id


getBoundingBox : { a | frameTraits : FrameTraits } -> Rectangle
getBoundingBox node =
    node.frameTraits.absoluteBoundingBox


getStrokeWidth : { a | frameTraits : FrameTraits } -> Float
getStrokeWidth node =
    node.frameTraits.strokeWeight
        |> Maybe.withDefault 0


toDetails : (FrameTraits -> List Elm.Expression) -> { a | frameTraits : FrameTraits } -> Details
toDetails getStyles node =
    let
        bbox =
            getBoundingBox node

        rbox =
            node.frameTraits.absoluteRenderBounds
                |> Maybe.withDefault bbox
    in
    { name = getName node
    , instanceName = ""
    , bbox = getBoundingBox node
    , renderedSize =
        { width = rbox.width
        , height = rbox.height
        }
    , strokeWidth = getStrokeWidth node
    , styles = getStyles node.frameTraits
    }


isHidden : { a | frameTraits : FrameTraits } -> Bool
isHidden { frameTraits } =
    Maybe.map not frameTraits.isLayerTrait.visible
        |> Maybe.withDefault False


isList : { a | frameTraits : FrameTraits } -> Bool
isList =
    getName >> nameIsList


nameIsList : String -> Bool
nameIsList =
    String.endsWith "List"

