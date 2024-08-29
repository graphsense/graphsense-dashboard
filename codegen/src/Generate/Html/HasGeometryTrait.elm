module Generate.Html.HasGeometryTrait exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Html.MinimalFillsTrait as MinimalFillsTrait
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint


toStyles : HasGeometryTrait -> List Elm.Expression
toStyles node =
    toBorder node.strokes
        |> m (Css.px >> Css.borderWidth) node.strokeWeight


toBorder : Maybe (List Paint) -> List Elm.Expression
toBorder paints =
    paints
        |> Maybe.andThen Paint.toStylesString
        |> Maybe.map
            (\color ->
                [ Css.borderStyle Css.solid
                , Css.call_.property (Elm.string "border-color") color
                ]
            )
        |> Maybe.withDefault []
