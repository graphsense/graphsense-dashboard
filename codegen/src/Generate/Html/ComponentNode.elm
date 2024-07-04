module Generate.Html.ComponentNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Generate.Html.FrameTraits as FrameTraits


toStyles : ComponentNode -> List Elm.Expression
toStyles node =
    FrameTraits.toStyles node.frameTraits
