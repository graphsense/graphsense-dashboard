module View.Pathfinder.Icons exposing (inIcon, outIcon)

import Css
import Css.Pathfinder as Css exposing (..)
import FontAwesome
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (disabled, id, src)
import Html.Styled.Lazy exposing (..)
import RecordSetter exposing (..)
import Theme.Html.Icons as Icons


iconsCss s =
    HA.css (Css.display Css.inline :: s) |> List.singleton


inIcon : Html msg
inIcon =
    Icons.iconsArrowDownThinWithAttributes (Icons.iconsArrowDownThinAttributes |> s_iconsArrowDownThin (iconsCss inIconStyle) |> s_vector (iconsCss inIconStyle)) {}



-- span [ inIconStyle |> toAttr ] [ FontAwesome.icon FontAwesome.signInAlt |> Html.fromUnstyled ]


outIcon : Html msg
outIcon =
    Icons.iconsArrowUpThinWithAttributes (Icons.iconsArrowUpThinAttributes |> s_iconsArrowUpThin (iconsCss outIconStyle) |> s_vector (iconsCss outIconStyle)) {}



-- span [ outIconStyle |> toAttr ] [ FontAwesome.icon FontAwesome.signOutAlt |> Html.fromUnstyled ]
