module View.Header exposing (header)

import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html, div, header, text)
import Html.Styled.Attributes as Html
import Msg exposing (Msg)
import Themes.Model exposing (Theme)


type alias Model =
    { theme : Theme
    , search : ()
    , user : ()
    }


css : Style -> Attribute msg
css custom =
    Html.css
        [ padding (px 10)
        , backgroundColor <| hex "000000"
        , displayFlex
        , flexDirection row
        , justifyContent spaceBetween
        , custom
        ]


header : Model -> Html Msg
header model =
    Html.header
        [ css model.theme.header
        ]
        [ div [] [ text "Dashboard" ]
        , div [] [ text "Search" ]
        , div [] [ text "User" ]
        ]
