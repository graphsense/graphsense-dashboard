module Generate.Html.HasLayoutTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)


toStyles : HasLayoutTrait -> List Elm.Expression
toStyles node =
    []
        |> m layoutSizingHorizontal node.layoutSizingHorizontal


layoutSizingHorizontal : LayoutSizingHorizontal -> Elm.Expression
layoutSizingHorizontal sizing =
    case sizing of
        LayoutSizingHorizontalFIXED ->
            Css.display Css.block

        LayoutSizingHorizontalFILL ->
            Css.display Css.block

        LayoutSizingHorizontalHUG ->
            Css.displayFlex
