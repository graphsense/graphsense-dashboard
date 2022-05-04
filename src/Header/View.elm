module Header.View exposing (header)

import Css exposing (..)
import Header.Css as Css
import Html.Styled exposing (Attribute, Html, div, header, img, text)
import Html.Styled.Attributes exposing (css, id, src)
import Model exposing (Msg(..), UserModel)
import Search.Model as Search
import Search.View as Search
import User.View as User
import View.Config exposing (Config)


type alias HeaderConfig =
    { latestBlocks : List ( String, Int )
    , search : Search.Model
    , user : UserModel
    }


header : Config -> HeaderConfig -> Html Msg
header vc hc =
    Html.Styled.header
        [ Css.header vc |> css
        , id "header"
        ]
        [ logo vc
        , Search.search vc
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
