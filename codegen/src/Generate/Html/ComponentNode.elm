module Generate.Html.ComponentNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Html.FrameTraits as FrameTraits
import Types exposing (ColorMap)


toStyles : ColorMap -> FrameTraits -> List Elm.Expression
toStyles colorMap node =
    FrameTraits.toStyles colorMap node
        ++ [ Css.position Css.relative ]
