module View exposing (view)

import Browser exposing (Document)
import Css exposing (..)
import Css.Reset
import Header.View as Header
import Hovercard
import Html.Attributes as Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Locale.View as Locale
import Model exposing (..)
import RemoteData
import User.View as User
import View.AddonsNav as AddonsNav
import View.Config exposing (Config)
import View.Css as Css
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
        ([ Header.header
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
            [ AddonsNav.nav vc
            , main_
                [ Css.main_ vc |> css
                ]
                [ Main.main_ vc model
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
