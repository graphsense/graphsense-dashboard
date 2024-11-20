module Generate.Svg.FrameTraits exposing (..)

import Api.Raw exposing (FrameTraits)
import Elm
import Generate.Common.FrameTraits as Common
import Generate.Svg.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait
import Types exposing (Details)


toStyles : FrameTraits -> List Elm.Expression
toStyles node =
    HasBlendModeAndOpacityTrait.toStyles node.hasBlendModeAndOpacityTrait


toDetails : { a | frameTraits : FrameTraits } -> Details
toDetails node =
    Common.toDetails toStyles node
