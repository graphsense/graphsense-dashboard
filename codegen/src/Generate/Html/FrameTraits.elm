module Generate.Html.FrameTraits exposing (..)

import Api.Raw exposing (FrameTraits)
import Elm
import Generate.Html.CornerTrait as CornerTrait
import Generate.Html.HasBlendModeAndOpacityTrait as HasBlendModeAndOpacityTrait
import Generate.Html.HasFramePropertiesTrait as HasFramePropertiesTrait
import Generate.Html.HasLayoutTrait as HasLayoutTrait


toCss : FrameTraits -> List Elm.Expression
toCss node =
    CornerTrait.toCss node.cornerTrait
        ++ HasBlendModeAndOpacityTrait.toCss node.hasBlendModeAndOpacityTrait
        ++ HasFramePropertiesTrait.toCss node.hasFramePropertiesTrait
        ++ HasLayoutTrait.toCss node.hasLayoutTrait
