module View.Button exposing (tool)

import Config.View exposing (Config)
import Css.View as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onMouseOver)
import Util.View exposing (aa)


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
