module Generate.Html.HasBlendModeAndOpacityTrait exposing (..)

import Api.Raw exposing (HasBlendModeAndOpacityTrait)
import Elm
import Gen.Css
import Generate.Util exposing (..)


toStyles : HasBlendModeAndOpacityTrait -> List Elm.Expression
toStyles node =
    []
        |> m opacity node.opacity


opacity : Float -> Elm.Expression
opacity =
    Gen.Css.num >> numberType >> Gen.Css.opacity
