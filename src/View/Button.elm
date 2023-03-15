module View.Button exposing (copyLink, tool)

import Config.View exposing (Config)
import Css.Browser as BCss
import Css.View as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onMouseOver)
import Util.View exposing (aa)
import View.Locale as Locale


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


copyLink : Config -> msg -> Html msg
copyLink vc cpyMsg =
    a
        [ BCss.propertyCopyLink vc True |> css
        , href "#"
        , onClick cpyMsg
        , title (Locale.string vc.locale "copy")
        ]
        [ FontAwesome.icon FontAwesome.copy
            |> Html.Styled.fromUnstyled
        ]
