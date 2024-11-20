module Generate.Html.HasGeometryTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Html.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import Types exposing (ColorMap)


toStyles : ColorMap -> HasGeometryTrait -> List Elm.Expression
toStyles colorMap node =
    toBorder colorMap node.strokes
        |> m (Css.px >> Css.borderWidth) node.strokeWeight


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
