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
        |> a2 (width node.minWidth) node.layoutSizingHorizontal node.size
        |> a2 (height node.minHeight) node.layoutSizingVertical node.size
        |> a (minWidth) node.minWidth
        |> a (minHeight) node.minHeight


minWidth : Float -> Maybe Elm.Expression
minWidth w = 
    if w == 0  then
        Nothing
    else
        Css.minWidth (Css.px w) |> Just

minHeight : Float -> Maybe Elm.Expression
minHeight w = 
    if w == 0  then
        Nothing
    else
        Css.minHeight (Css.px w) |> Just

width : Maybe Float -> LayoutSizingHorizontal -> Vector -> Maybe Elm.Expression
width minW sizing { x } =
    case sizing of
        LayoutSizingHorizontalFIXED ->
            if minW == Nothing || minW == Just 0 then
                x
                    |> Css.px
                    |> Css.width
                    |> Just

            else
                Nothing

        LayoutSizingHorizontalFILL ->
            Css.pct 100
                |> Css.width
                |> Just

        _ ->
            Nothing


height : Maybe Float -> LayoutSizingVertical -> Vector -> Maybe Elm.Expression
height minH sizing { y } =
    case sizing of
        LayoutSizingVerticalFIXED ->
            if minH == Nothing || minH == Just 0 then
                y
                    |> Css.px
                    |> Css.height
                    |> Just

            else
                Nothing

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
