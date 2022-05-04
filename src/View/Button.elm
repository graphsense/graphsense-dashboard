module View.Button exposing (tool)

import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onMouseOver)
import Util.View exposing (aa)
import View.Config exposing (Config)
import View.Css as Css


tool :
    Config
    ->
        { icon : FontAwesome.Icon
        }
    -> List (Attribute msg)
    -> Html msg
tool vc { icon } attr =
    FontAwesome.icon icon
        |> Html.Styled.fromUnstyled
        |> List.singleton
        |> span
            ([ Css.tool vc |> css ]
                ++ attr
            )
