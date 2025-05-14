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
        |> mm2 layoutPositioning node.layoutPositioning node.absoluteBoundingBox
        |> a3 (width node.minWidth) node.layoutGrow node.layoutSizingHorizontal node.absoluteBoundingBox
        |> a2 (height node.minHeight) node.layoutSizingVertical node.absoluteBoundingBox
        |> a (ifNotZero Css.minWidth) node.minWidth
        |> a (ifNotZero Css.minHeight) node.minHeight
        |> a (ifNotZero Css.maxWidth) node.maxWidth
        |> a (ifNotZero Css.maxHeight) node.maxHeight


ifNotZero : (Elm.Expression -> Elm.Expression) -> Float -> Maybe Elm.Expression
ifNotZero prop w =
    if w == 0 then
        Nothing

    else
        prop (Css.px w) |> Just


width : Maybe Float -> LayoutGrow -> LayoutSizingHorizontal -> Rectangle -> Maybe Elm.Expression
width minW grow sizing r =
    case sizing of
        LayoutSizingHorizontalFIXED ->
            if minW == Nothing || minW == Just 0 then
                r.width
                    |> Css.px
                    |> Css.width
                    |> Just

            else
                Nothing

        LayoutSizingHorizontalFILL ->
            Css.property "align-self" "stretch"
                |> Just

        _ ->
            Nothing


height : Maybe Float -> LayoutSizingVertical -> Rectangle -> Maybe Elm.Expression
height minH sizing r =
    case sizing of
        LayoutSizingVerticalFIXED ->
            if minH == Nothing || minH == Just 0 then
                r.height
                    |> Css.px
                    |> Css.height
                    |> Just

            else
                Nothing

        LayoutSizingVerticalFILL ->
            Css.property "align-self" "stretch"
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


layoutPositioning : LayoutPositioning -> Rectangle -> List Elm.Expression
layoutPositioning pos { x, y } =
    case pos of
        LayoutPositioningAUTO ->
            [ Css.position Css.relative
            ]

        LayoutPositioningABSOLUTE ->
            [ Css.position Css.absolute
            , Css.top <| Css.px y
            , Css.left <| Css.px x
            ]
