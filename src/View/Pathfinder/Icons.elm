module View.Pathfinder.Icons exposing (inIcon, outIcon)

import Css.Pathfinder as Css exposing (..)
import FontAwesome
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (disabled, id, src)
import Html.Styled.Lazy exposing (..)


inIcon : Html msg
inIcon =
    span [ inIconStyle |> toAttr ] [ FontAwesome.icon FontAwesome.signInAlt |> Html.fromUnstyled ]


outIcon : Html msg
outIcon =
    span [ outIconStyle |> toAttr ] [ FontAwesome.icon FontAwesome.signOutAlt |> Html.fromUnstyled ]
