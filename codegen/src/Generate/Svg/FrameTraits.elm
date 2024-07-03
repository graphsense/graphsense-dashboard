module Generate.Svg.FrameTraits exposing (..)

import Api.Raw exposing (FrameTraits)
import Elm
import Generate.Svg.CornerTrait as CornerTrait
import Generate.Svg.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait


toCss : FrameTraits -> List Elm.Expression
toCss node =
    CornerTrait.toCss node.cornerTrait
        ++ HasBlendModeAndOpacityTrait.toCss node.hasBlendModeAndOpacityTrait
