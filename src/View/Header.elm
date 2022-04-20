module View.Header exposing (header)

import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html, div, header, text)
import Html.Styled.Attributes as Html exposing (css)
import Msg exposing (Msg)
import View.Config exposing (Config)
import View.Css.Header as Css


type alias Model =
    { search : ()
    , user : ()
    }


header : Config -> Model -> Html Msg
header vc model =
    Html.header
        [ Css.header vc |> css
        ]
        [ logo vc
        , div [] [ text "Search" ]
        , div [] [ text "User" ]
        ]


logo : Config -> Html Msg
logo vc =
    Html.img
        [ Html.src vc.theme.logo
        , Css.headerLogo vc |> css
        ]
        []
