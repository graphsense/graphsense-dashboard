module Generate.Html.HasGeometryTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Html.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import Types exposing (ColorMap)


toStyles : ColorMap -> HasGeometryTrait -> Maybe StrokeWeights -> List Elm.Expression
toStyles =
    toBorder


toBorder : ColorMap -> HasGeometryTrait -> Maybe StrokeWeights -> List Elm.Expression
toBorder colorMap node strokeWeights =
    node.strokes
        |> Maybe.andThen (Paint.toStylesString colorMap)
        |> Maybe.map
            (\color ->
                [ Css.borderStyle Css.solid
                , Css.property "border-color" color
                ]
                    |> mm (borderWidth strokeWeights) node.strokeWeight
            )
        |> Maybe.withDefault []


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
