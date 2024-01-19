module View.Button exposing (actorLink, tool)

import Config.View exposing (Config)
import Css as CssStyled
import Css.Browser
import Css.View as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Route exposing (toUrl)
import Route.Graph as Route
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
            ((Css.tool vc |> css)
                :: attr
            )


actorLink : Config -> String -> String -> Html msg
actorLink vc id label =
    a
        [ href
            (Route.actorRoute id Nothing
                |> Route.graphRoute
                |> toUrl
            )
        , Css.link vc |> css
        ]
        [ span [] [ FontAwesome.icon FontAwesome.user |> Html.Styled.fromUnstyled ], span [ css [ CssStyled.paddingLeft (CssStyled.rem 0.2) ] ] [ text label ] ]
