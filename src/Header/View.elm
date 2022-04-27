module Header.View exposing (header)

import Css exposing (..)
import Header.Css as Css
import Html.Styled exposing (Attribute, Html, div, header, img, text)
import Html.Styled.Attributes exposing (css, id, src)
import Msg exposing (Msg(..))
import Search.Model as Search
import Search.View as Search
import View.Config exposing (Config)


type alias HeaderConfig =
    { latestBlocks : List ( String, Int )
    , search : Search.Model
    , user : ()
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
            []
            hc.search
            |> Html.Styled.map SearchMsg
        , div [] [ text "User" ]
        ]


logo : Config -> Html Msg
logo vc =
    img
        [ src vc.theme.logo
        , Css.headerLogo vc |> css
        ]
        []
