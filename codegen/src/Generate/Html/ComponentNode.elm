module Generate.Html.ComponentNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Html.FrameTraits as FrameTraits


toStyles : ComponentNode -> List Elm.Expression
toStyles node =
    FrameTraits.toStyles node.frameTraits
        ++ [ Css.position Css.relative ]
