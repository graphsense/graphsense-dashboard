module View.Pathfinder.Icons exposing (inIcon, outIcon)

import Css.Pathfinder exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as HA
import RecordSetter exposing (..)
import Theme.Html.Icons as Icons


inIcon : Html msg
inIcon =
    Icons.iconsArrowDownThinWithAttributes (Icons.iconsArrowDownThinAttributes |> s_iconsArrowDownThin (inIconStyle |> HA.css |> List.singleton) |> s_vector (inIconStyle |> HA.css |> List.singleton)) {}


outIcon : Html msg
outIcon =
    Icons.iconsArrowUpThinWithAttributes (Icons.iconsArrowUpThinAttributes |> s_iconsArrowUpThin (outIconStyle |> HA.css |> List.singleton) |> s_vector (outIconStyle |> HA.css |> List.singleton)) {}
