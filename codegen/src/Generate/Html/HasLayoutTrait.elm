module Generate.Html.HasLayoutTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import Tuple exposing (pair)


toStyles : HasLayoutTrait -> List Elm.Expression
toStyles node =
    [ Css.boxSizing Css.borderBox ]
        |> m layoutSizingHorizontal node.layoutSizingHorizontal
        |> a2 width node.layoutSizingHorizontal node.size
        |> a2 height node.layoutSizingVertical node.size


width : LayoutSizingHorizontal -> Vector -> Maybe Elm.Expression
width sizing { x, y } =
    case sizing of
        LayoutSizingHorizontalFIXED ->
            x
                |> Css.px
                |> Css.width
                |> Just

        LayoutSizingHorizontalFILL ->
            Css.pct 100
                |> Css.width
                |> Just

        _ ->
            Nothing


height : LayoutSizingVertical -> Vector -> Maybe Elm.Expression
height sizing { x, y } =
    case sizing of
        LayoutSizingVerticalFIXED ->
            y
                |> Css.px
                |> Css.height
                |> Just

        LayoutSizingVerticalFILL ->
            Css.pct 100
                |> Css.width
                |> Just

        _ ->
            Nothing


layoutSizingHorizontal : LayoutSizingHorizontal -> Elm.Expression
layoutSizingHorizontal sizing =
    case sizing of
        LayoutSizingHorizontalFIXED ->
            Css.displayFlex

        LayoutSizingHorizontalFILL ->
            Css.displayFlex

        LayoutSizingHorizontalHUG ->
            Css.displayFlex
