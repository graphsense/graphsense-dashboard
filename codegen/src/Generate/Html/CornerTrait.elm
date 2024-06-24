module Generate.Html.CornerTrait exposing (..)

import Api.Raw exposing (CornerTrait)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)


toCss : CornerTrait -> List Elm.Expression
toCss node =
    []
        |> m cornerRadius node.cornerRadius
        |> (++) (rectangleCornerRadii node.rectangleCornerRadii)


cornerRadius : Float -> Elm.Expression
cornerRadius =
    Css.px >> lengthType >> Css.borderRadius


rectangleCornerRadii : Maybe (List Float) -> List Elm.Expression
rectangleCornerRadii radii =
    case radii of
        Just (a :: b :: c :: d :: []) ->
            Css.borderRadius4
                (Css.px a |> lengthType)
                (Css.px b |> lengthType)
                (Css.px c |> lengthType)
                (Css.px d |> lengthType)
                |> List.singleton

        _ ->
            []
