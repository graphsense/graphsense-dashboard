module View exposing (view)

import Browser exposing (Document)
import Config.View exposing (Config)
import Css
import Css.Header as Css
import Css.Reset
import Css.View
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Model exposing (Auth(..), Model, Msg(..), Page(..))
import Model.Dialog as Dialog
import Plugin.View as Plugin exposing (Plugins)
import Route
import Route.Pathfinder as Pathfinder
import Util.Css
import Util.View exposing (hovercard)
import View.Dialog as Dialog
import View.Header as Header
import View.Locale as Locale
import View.Main as Main
import View.Notification as Notification
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
            ++ [ Notification.view vc model.notifications ]
        )


logo : Config -> Html Msg
logo vc =
    div
        [ css [ Css.padding4 (Css.px 5) (Css.px 15) (Css.px 5) (Css.px 15) ] ]
        [ img
            [ src <|
                if vc.lightmode then
                    vc.theme.logo_lightmode

                else
                    vc.theme.logo
            , Css.headerLogo vc |> css
            ]
            []
        ]


sidebar : Plugins -> Config -> Model key -> Html Msg
sidebar plugins vc model =
    let
        plugin_menu_items =
            Plugin.sidebar plugins model.plugins model.page vc
    in
    div
        [ Css.View.sidebar vc |> css
        ]
        ([ logo vc
         , hr [ Css.View.sidebarRule vc |> css ] []
         , FontAwesome.icon FontAwesome.home
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> a
                [ model.page == Home |> Css.View.sidebarIcon vc |> css
                , title (Locale.string vc.locale "Home")
                , Route.homeRoute
                    |> Route.toUrl
                    |> href
                ]
         , FontAwesome.icon FontAwesome.shareAlt
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> a
                [ model.page == Graph |> Css.View.sidebarIcon vc |> css
                , title (Locale.string vc.locale "Overview Network")
                , model.graph.route
                    |> Route.graphRoute
                    |> Route.toUrl
                    |> href
                ]
         , [ FontAwesome.icon FontAwesome.shareAlt
                |> Html.Styled.fromUnstyled
           , span
                [ css [ Css.fontSize <| Css.px 9 ]
                ]
                [ text "NEW" ]
           ]
            |> a
                [ model.page == Pathfinder |> Css.View.sidebarIcon vc |> css
                , title (Locale.string vc.locale "Pathfinder")
                , Route.pathfinderRoute Pathfinder.Root
                    |> Route.toUrl
                    |> href
                , css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    , Css.color (Css.rgba 151 219 207 1)
                    , Css.property "gap" "3px"
                    , Css.textDecoration Css.none
                    ]
                ]
         ]
            ++ (if List.length plugin_menu_items > 0 then
                    plugin_menu_items

                else
                    []
               )
            ++ [ hr [ Css.View.sidebarRule vc |> css ] []
               , a
                    [ Css.View.sidebarLink vc |> css
                    , Route.settingsRoute
                        |> Route.toUrl
                        |> href
                    ]
                    [ text (Locale.string vc.locale "Profile") ]
               , a
                    [ Css.View.sidebarLink vc |> css
                    , Route.settingsRoute
                        |> Route.toUrl
                        |> href
                    ]
                    [ text (Locale.string vc.locale "Settings") ]
               , a
                    [ Css.View.sidebarLink vc |> css
                    , Route.statsRoute
                        |> Route.toUrl
                        |> href
                    ]
                    [ text (Locale.string vc.locale "Statistics") ]
               , span [ Css.View.sidebarLink vc |> css, onClick UserClickedLightmode ]
                    [ text
                        (Locale.string vc.locale
                            (if vc.lightmode then
                                "Dark Mode"

                             else
                                "Light Mode"
                            )
                        )
                    ]
               , span [ Css.View.sidebarLink vc |> css, onClick UserClickedLogout ]
                    [ text
                        (Locale.string vc.locale "Logout")
                    ]
               ]
         -- ++ [ div [ model.page == Stats |> Css.View.sidebarIconsBottom vc |> css ]
         --         [-- FontAwesome.icon FontAwesome.chartPie
         --          --     |> Html.Styled.fromUnstyled
         --          --     |> List.singleton
         --          --     |> a
         --          --         [ model.page == Stats |> Css.View.sidebarIcon vc |> css
         --          --         , title (Locale.string vc.locale "Statistics")
         --          --         , Route.statsRoute
         --          --             |> Route.toUrl
         --          --             |> href
         --          --         ]
         --          --   div [ model.page == Stats |> Css.View.sidebarIcon vc |> css ] [ User.user vc model.user ]
         --         ]
         --    ]
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
