module Generate.Html.FrameTraits exposing (..)

import Api.Raw exposing (FrameTraits)
import Elm
import Generate.Common.FrameTraits as Common
import Generate.Html.CornerTrait as CornerTrait
import Generate.Html.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait
import Generate.Html.HasFramePropertiesTrait as HasFramePropertiesTrait
import Generate.Html.HasLayoutTrait as HasLayoutTrait
import Types exposing (Details)


toStyles : FrameTraits -> List Elm.Expression
toStyles node =
    CornerTrait.toStyles node.cornerTrait
        ++ HasBlendModeAndOpacityTrait.toStyles node.hasBlendModeAndOpacityTrait
        ++ HasFramePropertiesTrait.toStyles node.hasFramePropertiesTrait
        ++ HasLayoutTrait.toStyles node.hasLayoutTrait


toDetails : { a | frameTraits : FrameTraits } -> Details
toDetails node =
    Common.toDetails toStyles node
