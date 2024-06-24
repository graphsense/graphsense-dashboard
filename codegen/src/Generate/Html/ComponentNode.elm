module Generate.Html.ComponentNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Generate.Html.FrameTraits as FrameTraits


toCss : ComponentNode -> List Elm.Expression
toCss node =
    FrameTraits.toCss node.frameTraits
