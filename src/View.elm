module View exposing (view)

import Browser exposing (Document)
import Css exposing (..)
import Css.Reset
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Locale.View as Locale
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
    { title = Locale.string vc.locale "Iknaio Dashboard"
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
        [ Css.body vc |> css
        ]
        [ Header.header
            vc
            { search = model.search
            , user = model.user
            }
        , section
            [ Css.sectionBelowHeader vc |> css
            ]
            [ AddonsNav.nav vc
            , main_
                [ Css.main_ vc |> css
                ]
                [ Main.main_ vc model
                ]
            ]
        ]
