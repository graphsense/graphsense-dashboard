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
        |> a3 (width node.minWidth) node.layoutGrow node.layoutSizingHorizontal node.absoluteRenderBounds
        |> a2 (height node.minHeight) node.layoutSizingVertical node.absoluteRenderBounds
        |> a minWidth node.minWidth
        |> a minHeight node.minHeight


minWidth : Float -> Maybe Elm.Expression
minWidth w =
    if w == 0 then
        Nothing

    else
        Css.minWidth (Css.px w) |> Just


minHeight : Float -> Maybe Elm.Expression
minHeight w =
    if w == 0 then
        Nothing

    else
        Css.minHeight (Css.px w) |> Just


width : Maybe Float -> LayoutGrow -> LayoutSizingHorizontal -> Rectangle -> Maybe Elm.Expression
width minW grow sizing r =
    if grow == LayoutGrow1 then
        Css.num 1
                |> Css.flexGrow
                |> 
Just

        else
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
                    Css.pct 100
                        |> Css.width
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
            Css.pct 100
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
