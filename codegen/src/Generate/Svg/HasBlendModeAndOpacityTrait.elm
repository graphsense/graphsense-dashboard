module Generate.Svg.HasBlendModeAndOpacityTrait exposing (..)

import Api.Raw exposing (HasBlendModeAndOpacityTrait)
import Elm
import Gen.Css
import Generate.Util exposing (..)


toCss : HasBlendModeAndOpacityTrait -> List Elm.Expression
toCss node =
    []
        |> m opacity node.opacity


opacity : Float -> Elm.Expression
opacity =
    Gen.Css.num >> numberType >> Gen.Css.opacity
