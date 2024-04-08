module View exposing (view)

import Browser exposing (Document)
import Config.View exposing (Config)
import Css.Reset
import Css.View
import FontAwesome
import Hovercard
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Model exposing (Auth(..), Model, Msg(..), Page(..))
import Model.Dialog as Dialog
import Plugin.View as Plugin exposing (Plugins)
import Route
import Route.Graph
import Util.Css
import Util.View exposing (hovercard)
import View.Dialog as Dialog
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
    { title =
        Locale.string vc.locale "Iknaio Dashboard"
            :: Plugin.title plugins model.plugins vc
            |> List.reverse
            |> String.join " | "
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
            , hideSearch = model.page /= Graph
            }
         , section
            [ Css.View.sectionBelowHeader vc |> css
            ]
            [ sidebar plugins vc model
            , Main.view plugins vc model
            ]
         , footer
            [ Css.View.footer vc |> css
            ]
            [ Statusbar.view vc model.statusbar
            ]
         ]
            ++ hovercards plugins vc model
            ++ overlay plugins vc model
        )


sidebar : Plugins -> Config -> Model key -> Html Msg
sidebar plugins vc model =
    let
        plugin_menu_items =
            Plugin.sidebar plugins model.plugins model.page vc
    in
    div
        [ Css.View.sidebar vc |> css
        ]
        ([ FontAwesome.icon FontAwesome.home
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> a
                [ model.page == Home |> Css.View.sidebarIcon vc |> css
                , title (Locale.string vc.locale "Home")
                , Route.homeRoute
                    |> Route.toUrl
                    |> href
                ]
         , FontAwesome.icon FontAwesome.projectDiagram
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> a
                [ model.page == Pathfinder |> Css.View.sidebarIcon vc |> css
                , title (Locale.string vc.locale "Pathfinder")
                , model.pathfinder.route
                    |> Route.pathfinderRoute
                    |> Route.toUrl
                    |> href
                ]
         ]
            ++ (if List.length plugin_menu_items > 0 then
                    [ hr [ Css.View.sidebarRule vc |> css ] [] ]

                else
                    []
               )
            ++ plugin_menu_items
            ++ [ FontAwesome.icon FontAwesome.chartPie
                    |> Html.Styled.fromUnstyled
                    |> List.singleton
                    |> a
                        [ model.page == Stats |> Css.View.sidebarIconBottom vc |> css
                        , title (Locale.string vc.locale "Statistics")
                        , Route.statsRoute
                            |> Route.toUrl
                            |> href
                        ]
               ]
        )


hovercards : Plugins -> Config -> Model key -> List (Html Msg)
hovercards plugins vc model =
    model.user.hovercard
        |> Maybe.map
            (\hc ->
                User.hovercard plugins vc model model.user
                    |> List.map Html.Styled.toUnstyled
                    |> hovercard { vc | size = Nothing } hc (Util.Css.zIndexMainValue + 1)
                    |> List.singleton
            )
        |> Maybe.withDefault []


overlay : Plugins -> Config -> Model key -> List (Html Msg)
overlay plugins vc model =
    let
        ov onClickOutside =
            List.singleton
                >> div
                    [ Css.View.overlay vc |> css
                    , onClick onClickOutside
                    ]
                >> List.singleton
    in
    case model.user.auth of
        Unauthorized _ _ ->
            model.user.hovercard
                |> Maybe.map
                    (\hc ->
                        User.hovercard plugins vc model model.user
                            |> List.map Html.Styled.toUnstyled
                            |> hovercard { vc | size = Nothing } hc (Util.Css.zIndexMainValue + 1)
                    )
                |> Maybe.map (ov NoOp)
                |> Maybe.withDefault []

        _ ->
            case model.dialog of
                Just dialog ->
                    Dialog.view vc dialog
                        |> ov (Dialog.defaultMsg dialog)

                Nothing ->
                    []
