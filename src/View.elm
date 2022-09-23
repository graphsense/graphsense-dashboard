module View exposing (view)

import Browser exposing (Document)
import Browser.Dom as Dom
import Config.View exposing (Config)
import Css exposing (..)
import Css.Reset
import Css.View
import FontAwesome
import Hovercard
import Html
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Maybe.Extra
import Model exposing (Auth(..), Model, Msg(..), Page(..))
import Plugin.View as Plugin exposing (Plugins)
import RemoteData
import Route
import Route.Graph
import Util.View exposing (hovercard)
import View.Dialog as Dialog
import View.Graph.Search as Search
import View.Graph.Tag as Tag
import View.Header as Header
import View.Locale as Locale
import View.Main as Main
import View.Statusbar as Statusbar
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
        , onClick UserClickedLayout
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
            [ sidebar plugins vc model
            , main_
                [ Css.View.main_ vc |> css
                ]
                [ Main.main_ plugins vc model
                ]
            ]
         , footer
            [ Css.View.footer vc |> css
            ]
            [ Statusbar.view vc model.statusbar
            ]
         ]
            ++ hovercards plugins vc model
            ++ overlay vc model
        )


sidebar : Plugins -> Config -> Model key -> Html Msg
sidebar plugins vc model =
    div
        [ Css.View.sidebar vc |> css
        ]
        ([ FontAwesome.icon FontAwesome.home
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> a
                [ model.page == Stats |> Css.View.sidebarIcon vc |> css
                , Route.statsRoute
                    |> Route.toUrl
                    |> href
                ]
         , FontAwesome.icon FontAwesome.projectDiagram
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> a
                [ model.page == Graph |> Css.View.sidebarIcon vc |> css
                , Route.Graph.rootRoute
                    |> Route.graphRoute
                    |> Route.toUrl
                    |> href
                ]
         ]
            ++ Plugin.sidebar plugins model.plugins model.page vc
        )


hovercards : Plugins -> Config -> Model key -> List (Html Msg)
hovercards plugins vc model =
    (model.user.hovercardElement
        |> Maybe.map
            (\element ->
                User.hovercard vc model.user
                    |> List.map Html.Styled.toUnstyled
                    |> hovercard vc element
            )
        |> Maybe.withDefault []
    )
        ++ (model.graph.tag
                |> Maybe.map
                    (\tag ->
                        (Tag.inputHovercard plugins
                            vc
                            { entityConcepts = model.graph.config.entityConcepts
                            , abuseConcepts = model.graph.config.abuseConcepts
                            }
                            tag
                            |> Html.Styled.toUnstyled
                            |> List.singleton
                        )
                            |> List.map (Html.map GraphMsg)
                            |> hovercard vc tag.hovercardElement
                    )
                |> Maybe.withDefault []
           )
        ++ (model.graph.search
                |> Maybe.map
                    (\search ->
                        (Search.inputHovercard plugins vc search
                            |> Html.Styled.toUnstyled
                            |> List.singleton
                        )
                            |> List.map (Html.map GraphMsg)
                            |> hovercard vc search.element
                    )
                |> Maybe.withDefault []
           )
        ++ (model.graph.hovercardTBD
                |> Maybe.map
                    (\element ->
                        Html.text "To be done"
                            |> List.singleton
                            |> Html.div [ Html.Attributes.style "white-space" "nowrap", Html.Attributes.style "padding" "10px" ]
                            |> List.singleton
                            |> hovercard vc element
                    )
                |> Maybe.withDefault []
           )
        ++ Plugin.hovercards plugins model.plugins vc


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
                            , tickLength = 0
                            , borderColor = (vc.theme.hovercard vc.lightmode).borderColor
                            , backgroundColor = (vc.theme.hovercard vc.lightmode).backgroundColor
                            , borderWidth = (vc.theme.hovercard vc.lightmode).borderWidth
                            , overflow = "visible"
                            }
                            element
                            (Css.View.hovercard vc
                                |> List.map (\( k, v ) -> Html.Attributes.style k v)
                            )
                            (User.hovercard vc model.user |> List.map Html.Styled.toUnstyled)
                            |> Html.Styled.fromUnstyled
                    )
                |> Maybe.map ov
                |> Maybe.withDefault []

        _ ->
            case model.dialog of
                Just dialog ->
                    Dialog.view vc dialog
                        |> ov

                Nothing ->
                    []
