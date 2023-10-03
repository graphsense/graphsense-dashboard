module View.Header exposing (header)

import Config.View exposing (Config)
import Css.Header as Css
import Css.Search
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id, src)
import Model exposing (Msg(..), UserModel)
import Model.Search as Search
import Plugin.Model exposing (ModelState)
import Plugin.View exposing (Plugins)
import Util.View as View
import View.Search as Search
import View.User as User


type alias HeaderConfig =
    { search : Search.Model
    , user : UserModel
    , hideSearch : Bool
    }


header : Plugins -> ModelState -> Config -> HeaderConfig -> Html Msg
header plugins states vc hc =
    Html.Styled.header
        [ Css.header vc |> css
        , id "header"
        ]
        [ logo vc
        , if hc.hideSearch then
            View.none

          else
            Search.search plugins
                vc
                { css = Css.Search.textarea vc
                , resultsAsLink = True
                , multiline = True
                , showIcon = True
                }
                hc.search
                |> Html.Styled.map SearchMsg
        , User.user vc hc.user
        ]


logo : Config -> Html Msg
logo vc =
    div
        [ Css.headerLogoWrap vc |> css ]
        [ img
            [ src <|
                if vc.lightmode then
                    vc.theme.logo_lightmode

                else
                    vc.theme.logo
            , Css.headerLogo vc |> css
            ]
            []
        , h1 [ Css.headerTitle vc |> css ] [ text "Pathfinder" ]
        ]
