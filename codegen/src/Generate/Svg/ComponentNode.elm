module Generate.Svg.ComponentNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Generate.Svg.FrameTraits as FrameTraits


toCss : ComponentNode -> List Elm.Expression
toCss node =
    FrameTraits.toCss node.frameTraits
