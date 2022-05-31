module View.Header exposing (header)

import Config.View exposing (Config)
import Css exposing (..)
import Css.Header as Css
import Html.Styled exposing (Attribute, Html, div, header, img, text)
import Html.Styled.Attributes exposing (css, id, src)
import Model exposing (Msg(..), UserModel)
import Model.Search as Search
import Plugin exposing (Plugins)
import Ports
import View.Search as Search
import View.User as User


type alias HeaderConfig =
    { latestBlocks : List ( String, Int )
    , search : Search.Model
    , user : UserModel
    }


header : Plugins -> Config -> HeaderConfig -> Html Msg
header plugins vc hc =
    Html.Styled.header
        [ Css.header vc |> css
        , id "header"
        ]
        [ logo vc
        , Search.search plugins
            vc
            { latestBlocks = hc.latestBlocks
            }
            hc.search
            |> Html.Styled.map SearchMsg
        , User.user vc hc.user
        ]


logo : Config -> Html Msg
logo vc =
    img
        [ src vc.theme.logo
        , Css.headerLogo vc |> css
        ]
        []
