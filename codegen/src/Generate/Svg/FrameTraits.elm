module Generate.Svg.FrameTraits exposing (..)

import Api.Raw exposing (FrameTraits)
import Elm
import Generate.Svg.CornerTrait as CornerTrait
import Generate.Svg.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait
import Generate.Common.FrameTraits as Common
import Types exposing (Details)


toStyles : FrameTraits -> List Elm.Expression
toStyles node =
    CornerTrait.toStyles node.cornerTrait
        ++ HasBlendModeAndOpacityTrait.toStyles node.hasBlendModeAndOpacityTrait


toDetails : { a | frameTraits : FrameTraits } -> Details
toDetails node =
    Common.toDetails toStyles node
