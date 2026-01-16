module View exposing (sidebarMenuItem, view)

import Browser exposing (Document)
import Config.View exposing (Config)
import Css
import Css.Reset
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Model exposing (Auth(..), Model, Msg(..), NavbarSubMenu, NavbarSubMenuType(..), Page(..))
import Model.Dialog as Dialog
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import Route
import Route.Pathfinder as Pathfinder
import Theme.Colors
import Theme.ColorsDark
import Theme.Html.GraphComponents as GraphComponents
import Theme.Html.Icons as Icons
import Theme.Html.Navbar as Nb
import Util.Css
import Util.View exposing (fixFillRule, hovercard, onClickWithStop)
import View.Dialog as Dialog
import View.Header as Header
import View.Locale as Locale
import View.Main as Main
import View.Notification as Notification
import View.Pathfinder.ContextMenuItem as ContextMenuItem
import View.Pathfinder.Tooltip as Tooltip
import View.Statusbar as Statusbar
import View.User as User


view :
    Plugins
    -> Config
    -> Model key
    -> Document Msg
view plugins vc model =
    { title =
        Locale.string vc.locale "Iknaio Analytics Platform"
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
           body { overflow: hidden; }
           input { border: 0; }
           """ ] |> toUnstyled
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
            ++ Notification.view vc model.notifications
            :: (model.tooltip
                    |> Maybe.map (Tooltip.view plugins model vc)
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
               )
        )


navbarSubMenuView : Config -> Model key -> NavbarSubMenu -> Html Msg
navbarSubMenuView vc model { type_ } =
    let
        fixedWidth =
            180
    in
    div
        [ [ Css.left (Css.px (Nb.navbarMenuNew_details.renderedWidth - 5))
          , Css.top (Css.px 0)
          , Css.position Css.absolute
          , Css.zIndex (Css.int (Util.Css.zIndexMainValue + 1))
          ]
            |> css
        , onClickWithStop UserClosesNavbarSubMenu
        , Util.View.noTextSelection
        ]
        ((case type_ of
            NavbarMore ->
                GraphComponents.rightClickMenu
                    { shortcutList =
                        [ { link = model.graph.route |> Route.graphRoute |> Route.toUrl
                          , icon = Icons.iconsPathfinderStateDefault {}
                          , text1 = "Pathfinder 1.0"
                          , text2 = Nothing
                          , blank = False
                          }
                            |> ContextMenuItem.initLink2
                            |> ContextMenuItem.view vc
                        ]
                    , pluginsList =
                        [ { link = "https://www.iknaio.com/learning#pathfinder20"
                          , icon = Icons.iconsVideoS {}
                          , text1 = "Watch tutorials"
                          , text2 = Nothing
                          , blank = True
                          }
                            |> ContextMenuItem.initLink2
                            |> ContextMenuItem.view vc
                        , { link = "https://www.iknaio.com/services"
                          , icon = Icons.iconsGoToS {}
                          , text1 = "All our services"
                          , text2 = Nothing
                          , blank = True
                          }
                            |> ContextMenuItem.initLink2
                            |> ContextMenuItem.view vc
                        ]
                    }
                    {}
         )
            |> List.singleton
        )


sidebarMenuItemWithSubMenu : Config -> Model key -> Msg -> Html Msg -> String -> Bool -> Bool -> Html Msg
sidebarMenuItemWithSubMenu vc model toggleMsg img label selected new =
    div
        [ onClickWithStop toggleMsg
        , [ Css.position Css.relative ] |> css
        , Util.View.pointer
        ]
        (sidebarMenuItemPlain img (Locale.string vc.locale label) selected new
            :: (model.navbarSubMenu
                    |> Maybe.map (navbarSubMenuView vc model >> List.singleton)
                    |> Maybe.withDefault []
               )
        )


sidebarMenuItem : Html msg -> String -> String -> Bool -> String -> Html msg
sidebarMenuItem img label titleStr selected link =
    sidebarMenuItemWithNewParam img label titleStr selected link False


sidebarMenuItemPlain : Html msg -> String -> Bool -> Bool -> Html msg
sidebarMenuItemPlain img label selected new =
    let
        ifNewAddEvenOdd =
            if new then
                fixFillRule
                    |> List.singleton
                    |> Rs.s_subtract

            else
                identity
    in
    Nb.navbarProductItemWithAttributes
        (Nb.navbarProductItemAttributes
            |> Rs.s_pathfinder [ [ Css.hover Nb.navbarProductItemStateHoverPathfinder_details.styles ] |> css ]
            |> Rs.s_root
                (if not selected then
                    [ Css.hover
                        (Util.Css.overrideBlack Theme.Colors.sidebarHovered
                            :: Nb.navbarProductItemStateHover_details.styles
                        )
                    ]
                        |> css
                        |> List.singleton

                 else
                    []
                )
            |> ifNewAddEvenOdd
        )
        { root =
            { iconInstance = img
            , productLabel = label
            , newLabelVisible = new
            , state =
                if not selected then
                    Nb.NavbarProductItemStateNeutral

                else
                    Nb.NavbarProductItemStateSelected
            }
        }


sidebarMenuItemWithNewParam : Html msg -> String -> String -> Bool -> String -> Bool -> Html msg
sidebarMenuItemWithNewParam img label titleStr selected link new =
    sidebarMenuItemPlain img label selected new
        |> (\x ->
                if selected then
                    x

                else
                    x
                        |> List.singleton
                        |> a
                            [ title titleStr
                            , link
                                |> href
                            , css [ Css.textDecoration Css.none ]
                            ]
           )


sidebar : Plugins -> Config -> Model key -> Html Msg
sidebar plugins vc model =
    let
        products =
            -- [ sidebarMenuItem (Nb.iconsPathfinder10 {}) "Pathfinder" "Pathfinder" (model.page == Graph) (model.graph.route |> Route.graphRoute |> Route.toUrl)
            sidebarMenuItemWithNewParam (Nb.iconsPathfinder10 {}) "Pathfinder" "Pathfinder" (model.page == Pathfinder) (Route.pathfinderRoute Pathfinder.Root |> Route.toUrl) False
                :: Plugin.sidebar plugins model.plugins model.page vc
                ++ [ sidebarMenuItemWithSubMenu vc model (UserToggledNavbarSubMenu NavbarMore) (Nb.iconsMoreHorizL {}) (Locale.string vc.locale "More") False False
                   ]

        statsLinkItem =
            Nb.textItremWithAttributes
                (Nb.textItremAttributes
                    |> Rs.s_statistics
                        [ Css.hover Nb.textItremStateSelectedStatistics_details.styles
                            |> List.singleton
                            |> css
                        ]
                )
                { root =
                    { state =
                        if model.page == Stats then
                            Nb.TextItremStateSelected

                        else
                            Nb.TextItremStateNeutral
                    , textLabel = Locale.string vc.locale "statistics"
                    }
                }

        statisticsLink =
            statsLinkItem
                |> List.singleton
                |> a
                    [ Route.statsRoute
                        |> Route.toUrl
                        |> href
                    , Css.none |> Css.textDecoration |> List.singleton |> css
                    ]

        settingsLink =
            Nb.iconsSettingsLargeWithAttributes
                (Nb.iconsSettingsLargeAttributes
                    |> Rs.s_root
                        [ Css.hover
                            [ Css.property Theme.Colors.sidebarNeutral_name Theme.Colors.sidebarHovered
                            ]
                            |> List.singleton
                            |> css
                        ]
                )
                { root =
                    { state =
                        if model.page == Settings then
                            Nb.IconsSettingsLargeStateSelected

                        else
                            Nb.IconsSettingsLargeStateNeutral
                    }
                }
                |> List.singleton
                |> a
                    [ title (Locale.string vc.locale "settings")
                    , (Route.settingsRoute |> Route.toUrl)
                        |> href
                    , Css.none |> Css.textDecoration |> List.singleton |> css
                    ]
    in
    Nb.navbarMenuNewWithInstances
        (Nb.navbarMenuNewAttributes
            |> Rs.s_root
                (model.height |> toFloat |> Css.px |> Css.height |> List.singleton |> css |> List.singleton)
            |> Rs.s_navbarIknaioLogo
                [ [ Css.pointer |> Css.cursor
                  , Css.pointerEventsAll
                  ]
                    |> css
                , onClick UserClickedNavHome
                ]
        )
        (Nb.navbarMenuNewInstances
         -- |> Rs.s_statistics (Just statisticsLink)
         -- |> Rs.s_help (Just Util.View.none)
        )
        { productsList = products }
        { root =
            { helpLabel = ""

            -- , iconInstance = sidebarMenuItem (Nb.iconsSettingsLargeStateNeutral {}) "" (Locale.string vc.locale "settings") (model.page == Settings) (Route.settingsRoute |> Route.toUrl)
            , iconInstance = Util.View.none
            , statisticsLabel = ""
            }
        , statisticsItrem = { variant = statisticsLink }
        , helpItrem = { variant = Util.View.none }
        , iconsSettingsLarge = { variant = settingsLink }
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
                    , onClick (UserClickedOutsideDialog onClickOutside)
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
                    Dialog.view plugins model.plugins vc dialog
                        |> ov (Dialog.defaultMsg dialog)

                Nothing ->
                    []
