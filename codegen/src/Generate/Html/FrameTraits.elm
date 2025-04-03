module Generate.Html.FrameTraits exposing (..)

import Api.Raw exposing (FrameTraits)
import Elm
import Generate.Common.FrameTraits as Common
import Generate.Html.CornerTrait as CornerTrait
import Generate.Html.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait
import Generate.Html.HasEffectsTrait as HasEffectsTrait
import Generate.Html.HasFramePropertiesTrait as HasFramePropertiesTrait
import Generate.Html.HasGeometryTrait as HasGeometryTrait
import Generate.Html.HasLayoutTrait as HasLayoutTrait
import Types exposing (ColorMap, Details)


toStyles : ColorMap -> FrameTraits -> List Elm.Expression
toStyles colorMap node =
    CornerTrait.toStyles node.cornerTrait
        ++ HasBlendModeAndOpacityTrait.toStyles node.hasBlendModeAndOpacityTrait
        ++ HasLayoutTrait.toStyles node.hasLayoutTrait
        ++ HasFramePropertiesTrait.toStyles colorMap node.hasFramePropertiesTrait
        ++ HasGeometryTrait.toStyles colorMap node.hasGeometryTrait node.individualStrokeWeights
        ++ HasEffectsTrait.toStyles colorMap node.hasEffectsTrait


toDetails : ColorMap -> { a | frameTraits : FrameTraits } -> Details
toDetails colorMap node =
    Common.toDetails (toStyles colorMap) node
