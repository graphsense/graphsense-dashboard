module Generate.Html.HasLayoutTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import Tuple exposing (pair)


toStyles : HasLayoutTrait -> HasFramePropertiesTrait -> List Elm.Expression
toStyles node fpt =
    []
        |> m layoutSizingHorizontal node.layoutSizingHorizontal
        |> a (width fpt) (Maybe.map2 pair node.layoutSizingHorizontal node.size)
        |> a (height fpt) (Maybe.map2 pair node.layoutSizingVertical node.size)


width : HasFramePropertiesTrait -> ( LayoutSizingHorizontal, Vector ) -> Maybe Elm.Expression
width fpt ( sizing, { x, y } ) =
    case sizing of
        LayoutSizingHorizontalFIXED ->
            x
                - (fpt.paddingLeft |> Maybe.withDefault 0)
                - (fpt.paddingRight |> Maybe.withDefault 0)
                |> Css.px
                |> Css.width
                |> Just

        _ ->
            Nothing


height : HasFramePropertiesTrait -> ( LayoutSizingVertical, Vector ) -> Maybe Elm.Expression
height fpt ( sizing, { x, y } ) =
    case sizing of
        LayoutSizingVerticalFIXED ->
            y
                - (fpt.paddingTop |> Maybe.withDefault 0)
                - (fpt.paddingBottom |> Maybe.withDefault 0)
                |> Css.px
                |> Css.height
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
