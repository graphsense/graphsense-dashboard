module View exposing (view)

import Browser exposing (Document)
import Css exposing (..)
import Css.Reset
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Plugin exposing (Plugin)
import View.AddonsNav as AddonsNav
import View.Config exposing (Config)
import View.Css as Css
import View.Header as Header
import View.Main as Main


view :
    Config
    -> Model key
    -> Document Msg
view vc model =
    { title = vc.getString "Iknaio Dashboard"
    , body =
        [ Css.Reset.meyerV2 |> toUnstyled
        , node "style" [] [ text vc.theme.custom ] |> toUnstyled
        , body vc model |> toUnstyled
        ]
    }


body :
    Config
    -> Model key
    -> Html Msg
body vc model =
    div
        [ css
            [ Css.height <| vh 100
            , displayFlex
            , flexDirection column
            , overflow Css.hidden
            , vc.theme.body
            ]
        ]
        [ Header.header
            vc
            { search = model.search
            , user = model.user
            }
        , section
            [ css
                [ displayFlex
                , flexDirection row
                , flexGrow (num 1)
                ]
            ]
            [ AddonsNav.nav vc
            , main_
                [ Css.main_ vc |> css
                ]
                [ Main.main_ vc model
                ]
            ]
        ]
