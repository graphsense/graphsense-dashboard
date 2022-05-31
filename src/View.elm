module View exposing (view)

import Browser exposing (Document)
import Config.View exposing (Config)
import Css exposing (..)
import Css.Reset
import Css.View as Css
import Hovercard
import Html.Attributes as Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (Model, Msg(..))
import Plugin as Plugin exposing (Plugins)
import RemoteData
import View.Header as Header
import View.Locale as Locale
import View.Main as Main
import View.User as User


view :
    Plugins
    -> Config
    -> Model key
    -> Document Msg
view plugins vc model =
    { title = Locale.string vc.locale "Iknaio Dashboard"
    , body =
        [ Css.Reset.meyerV2 |> toUnstyled
        , node "style" [] [ text vc.theme.custom ] |> toUnstyled
        , body plugins vc model |> toUnstyled
        ]
    }


body :
    Plugins
    -> Config
    -> Model key
    -> Html Msg
body plugins vc model =
    div
        [ Css.body vc |> css
        ]
        ([ Header.header
            plugins
            model.plugins
            vc
            { search = model.search
            , user = model.user
            , latestBlocks =
                model.stats
                    |> RemoteData.map .currencies
                    |> RemoteData.withDefault []
                    |> List.map (\{ name, noBlocks } -> ( name, noBlocks - 1 ))
            }
         , section
            [ Css.sectionBelowHeader vc |> css
            ]
            [ main_
                [ Css.main_ vc |> css
                ]
                [ Main.main_ plugins vc model
                ]
            ]
         ]
            ++ hovercards vc model
        )


hovercards : Config -> Model key -> List (Html Msg)
hovercards vc model =
    model.user.hovercardElement
        |> Maybe.map
            (\element ->
                Hovercard.hovercard
                    { maxWidth = 300
                    , maxHeight = 500
                    , tickLength = 16
                    , borderColor = vc.theme.hovercard.borderColor
                    , backgroundColor = vc.theme.hovercard.backgroundColor
                    , borderWidth = vc.theme.hovercard.borderWidth
                    }
                    element
                    (Css.hovercard vc
                        |> List.map (\( k, v ) -> Html.style k v)
                    )
                    (User.hovercard vc model.user |> List.map Html.Styled.toUnstyled)
                    |> Html.Styled.fromUnstyled
                    |> List.singleton
            )
        |> Maybe.withDefault []
