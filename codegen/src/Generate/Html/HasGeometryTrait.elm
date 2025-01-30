module Generate.Html.HasGeometryTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Html.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import Types exposing (ColorMap)


toStyles : ColorMap -> HasGeometryTrait -> Maybe StrokeWeights -> List Elm.Expression
toStyles colorMap node strokeWeights =
    toBorder colorMap node.strokes
        |> mm (borderWidth strokeWeights) node.strokeWeight


borderWidth : Maybe StrokeWeights -> Float -> List Elm.Expression
borderWidth strokeWeights strokeWeight =
    strokeWeights
        |> Maybe.map
            (\{ top, bottom, left, right } ->
                [ Css.px top |> Css.borderTopWidth
                , Css.px bottom |> Css.borderBottomWidth
                , Css.px left |> Css.borderLeftWidth
                , Css.px right |> Css.borderRightWidth
                ]
            )
        |> Maybe.withDefault (Css.px strokeWeight |> Css.borderWidth |> List.singleton)


toBorder : ColorMap -> Maybe (List Paint) -> List Elm.Expression
toBorder colorMap paints =
    paints
        |> Maybe.andThen (Paint.toStylesString colorMap)
        |> Maybe.map
            (\color ->
                [ Css.borderStyle Css.solid
                , Css.property "border-color" color
                ]
            )
        |> Maybe.withDefault []
