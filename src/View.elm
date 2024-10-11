module View exposing (sidebarMenuItem, view)

import Browser exposing (Document)
import Config.View exposing (Config)
import Css
import Css.Reset
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Model exposing (Auth(..), Model, Msg(..), Page(..))
import Model.Dialog as Dialog
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import Route
import Route.Pathfinder as Pathfinder
import Theme.Colors
import Theme.ColorsDark
import Theme.Html.Icons as Icons
import Theme.Html.Navbar as Nb
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
        , (if vc.lightmode then
            Theme.Colors.style

           else
            Theme.ColorsDark.style
          )
            |> toUnstyled
        , node "style" [] [ text """
            * {
                transition: color 0.5s, background-color 0.5s;
            }""" ] |> toUnstyled
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

sidebarMenuItem : Html msg -> String -> String -> Bool -> String -> Html msg
sidebarMenuItem img label titleStr selected link =
    if not selected then
        Nb.navbarProductItemStateNeutralWithInstances
            (Nb.navbarProductItemStateNeutralAttributes
                |> Rs.s_pathfinder [ [ Css.hover Nb.navbarProductItemStateHoverPathfinder_details.styles ] |> css ]
                |> Rs.s_stateNeutral
                    [ [ Css.hover Nb.navbarProductItemStateHover_details.styles
                      , Nb.navbarMenuNewNavbarIknaioLogoNavbarIknaioLogo_details.width |> Css.px |> Css.width
                      ]
                        |> css
                    ]
            )
            Nb.navbarProductItemStateNeutralInstances
            { stateNeutral = { iconInstance = img, productLabel = label } }
            |> List.singleton
            |> a
                [ title titleStr
                , link
                    |> href
                , css [ Css.textDecoration Css.none ]
                ]

    else
        Nb.navbarProductItemStateSelectedWithInstances
            (Nb.navbarProductItemStateSelectedAttributes
                |> Rs.s_stateSelected [ [ Nb.navbarMenuNewNavbarIknaioLogoNavbarIknaioLogo_details.width |> Css.px |> Css.width ] |> css ]
            )
            Nb.navbarProductItemStateSelectedInstances
            { stateSelected = { iconInstance = img, productLabel = label } }


sidebar : Plugins -> Config -> Model key -> Html Msg
sidebar plugins vc model =
    let
        plugin_menu_items =
            Plugin.sidebar plugins model.plugins model.page vc

        products =
            div [ Nb.navbarMenuNewProducts_details.styles |> css ]
                ([ sidebarMenuItem (Nb.iconsPathfinderNew {}) "Network" "Network" (model.page == Graph) (model.graph.route |> Route.graphRoute |> Route.toUrl)
                 , sidebarMenuItem (Nb.iconsPathfinderNew {}) "Pathfinder" "Pathfinder" (model.page == Pathfinder) (Route.pathfinderRoute Pathfinder.Root |> Route.toUrl)
                 ]
                    ++ (if List.length plugin_menu_items > 0 then
                            plugin_menu_items

                        else
                            []
                       )
                )

        statisticsLink =
            Locale.string vc.locale "Statistics"
                |> text
                |> List.singleton
                |> a
                    [ Route.statsRoute
                        |> Route.toUrl
                        |> href
                    , Nb.navbarMenuNewStatistics_details.styles |> css
                    , [ Css.textDecoration Css.none ] |> css
                    ]
    in
    Nb.navbarMenuNewWithInstances
        (Nb.navbarMenuNewAttributes
            |> Rs.s_navbarMenuNew [ [ model.height |> toFloat |> Css.px |> Css.height ] |> css ]
        )
        (Nb.navbarMenuNewInstances
            |> Rs.s_products (Just products)
            |> Rs.s_statistics (Just statisticsLink)
            |> Rs.s_help (Just Util.View.none)
        )
        { caseconnectItem = { variant = Util.View.none }
        , navbarMenuNew =
            { helpLabel = ""
            , iconInstance = sidebarMenuItem (Icons.iconsSettingsLarge {}) "" (Locale.string vc.locale "Settings") (model.page == Settings) (Route.settingsRoute |> Route.toUrl)
            , statisticsLabel = ""
            }
        , pathfinderItem = { variant = Util.View.none }
        , quicklockItem = { variant = Util.View.none }
        , taxreportItem = { variant = Util.View.none }
        }


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
