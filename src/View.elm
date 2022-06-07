module View exposing (view)

import Browser exposing (Document)
import Config.View exposing (Config)
import Css exposing (..)
import Css.Reset
import Css.View
import Hovercard
import Html.Attributes as Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Maybe.Extra
import Model exposing (Auth(..), Model, Msg(..))
import Plugin as Plugin exposing (Plugins)
import RemoteData
import View.Graph.Tag as Tag
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
        [ Css.View.body vc |> css
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
            [ Css.View.sectionBelowHeader vc |> css
            ]
            [ main_
                [ Css.View.main_ vc |> css
                ]
                [ Main.main_ plugins vc model
                ]
            ]
         ]
            ++ hovercards plugins vc model
            ++ overlay vc model
        )


hovercards : Plugins -> Config -> Model key -> List (Html Msg)
hovercards plugins vc model =
    (model.user.hovercardElement
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
                    (Css.View.hovercard vc
                        |> List.map (\( k, v ) -> Html.style k v)
                    )
                    (User.hovercard vc model.user |> List.map Html.Styled.toUnstyled)
                    |> Html.Styled.fromUnstyled
                    |> List.singleton
            )
        |> Maybe.withDefault []
    )
        ++ (model.graph.tag
                |> Maybe.map
                    (\tag ->
                        Hovercard.hovercard
                            { maxWidth = 300
                            , maxHeight = 500
                            , tickLength = 16
                            , borderColor = vc.theme.hovercard.borderColor
                            , backgroundColor = vc.theme.hovercard.backgroundColor
                            , borderWidth = vc.theme.hovercard.borderWidth
                            }
                            tag.hovercardElement
                            (Css.View.hovercard vc
                                |> List.map (\( k, v ) -> Html.style k v)
                            )
                            (Tag.inputHovercard plugins
                                vc
                                { entityConcepts = model.entityConcepts
                                , abuseConcepts = model.abuseConcepts
                                }
                                tag
                                |> Html.Styled.toUnstyled
                                |> List.singleton
                            )
                            |> Html.Styled.fromUnstyled
                            |> Html.Styled.map GraphMsg
                            |> List.singleton
                    )
                |> Maybe.withDefault []
           )


overlay : Config -> Model key -> List (Html Msg)
overlay vc model =
    let
        ov =
            List.singleton
                >> div
                    [ Css.View.overlay vc |> css
                    ]
                >> List.singleton
    in
    case model.user.auth of
        Unauthorized _ _ ->
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
                            (Css.View.hovercard vc
                                |> List.map (\( k, v ) -> Html.style k v)
                            )
                            (User.hovercard vc model.user |> List.map Html.Styled.toUnstyled)
                            |> Html.Styled.fromUnstyled
                    )
                |> Maybe.map ov
                |> Maybe.withDefault []

        _ ->
            []
