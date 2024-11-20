module Generate.Svg.ComponentNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Generate.Svg.FrameTraits as FrameTraits


toStyles : ComponentNode -> List Elm.Expression
toStyles node =
    FrameTraits.toStyles node.frameTraits
