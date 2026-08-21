module View exposing (view)

import Browser exposing (Document)
import Config.View exposing (Config)
import Css
import Css.Reset
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Model exposing (Model, Msg(..), NavbarSubMenuType(..), Page(..))
import Model.Dialog as Dialog
import Plugin.View as Plugin
import RecordSetter as Rs
import Route
import Route.Pathfinder as Pathfinder
import Theme.Colors
import Theme.ColorsDark
import Theme.Html.Navbar as Nb
import Util.View
import View.Dialog as Dialog
import View.Header as Header
import View.Locale as Locale
import View.Main as Main
import View.Notification as Notification
import View.Sidebar as Sidebar
import View.Statusbar as Statusbar


view :
    Config
    -> Model key
    -> Document Msg
view vc model =
    { title =
        Locale.string vc.locale "Iknaio Analytics Platform"
            :: Plugin.title model.plugins vc
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
        , node "style" [] [ text """
           .gs-markdown { overflow-wrap: break-word; }
           .gs-markdown h1,
           .gs-markdown h2,
           .gs-markdown h3,
           .gs-markdown h4,
           .gs-markdown h5,
           .gs-markdown h6 { display: inline; font-weight: bold; }
           .gs-markdown p { margin: 0 0 1em 0; }
           .gs-markdown ul,
           .gs-markdown ol { margin: 0 0 1em 0; padding-left: 2em; }
           .gs-markdown li { margin: 0.5em 0; }
           .gs-markdown strong,
           .gs-markdown b { font-weight: bold; }
           .gs-markdown em,
           .gs-markdown i { font-style: italic; }
           """ ] |> toUnstyled
        , body vc model |> toUnstyled
        ]
    }


body :
    Config
    -> Model key
    -> Html Msg
body vc model =
    div
        [ Css.View.body vc |> css
        , onClick UserClickedLayout
        ]
        ([ Header.header
            model.plugins
            vc
            { search = model.search
            , user = model.user
            , hideSearch = True
            }
         , section
            [ Css.View.sectionBelowHeader vc |> css
            ]
            [ sidebar vc model
            , Main.view vc model
            ]
         , footer
            [ Css.View.footer vc |> css
            ]
            [ Statusbar.view vc model.statusbar
            ]
         ]
            ++ overlay vc model
            ++ (Notification.view vc model.notifications
                    :: Maybe.withDefault [] (Plugin.tooltip model.plugins vc)
               )
        )


sidebar : Config -> Model key -> Html Msg
sidebar vc model =
    let
        products =
            -- [ sidebarMenuItem (Nb.iconsPathfinder10 {}) "Pathfinder" "Pathfinder" (model.page == Graph) (model.graph.route |> Route.graphRoute |> Route.toUrl)
            Sidebar.sidebarMenuItemWithNewParam (Nb.iconsPathfinder10 {}) "Pathfinder" "Pathfinder" (model.page == Pathfinder) (Route.pathfinderRoute Pathfinder.Root |> Route.toUrl) False
                :: Plugin.sidebar model.plugins model.page vc
                ++ [ Sidebar.sidebarMenuItemWithSubMenu vc model (UserToggledNavbarSubMenu NavbarMore) (Nb.iconsMoreHorizL {}) (Locale.string vc.locale "More") False False
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
        , iconsSettingsLarge = { variant = settingsLink }
        }


overlay : Config -> Model key -> List (Html Msg)
overlay vc model =
    let
        ov placement onClickOutside =
            List.singleton
                >> div
                    [ Css.View.overlay placement vc |> css
                    , onClick (UserClickedOutsideDialog onClickOutside)
                    ]
                >> List.singleton
    in
    case model.dialog of
        Just dialog ->
            Dialog.view model.plugins vc dialog
                |> ov (Dialog.placement dialog) (Dialog.defaultMsg dialog)

        Nothing ->
            []
