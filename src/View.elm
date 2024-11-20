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
           body {
               overflow: hidden;
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
                      ]
                        |> css
                    ]
            )
            Nb.navbarProductItemStateNeutralInstances
            { stateNeutral = { iconInstance = img, productLabel = label, newLabelVisible = False } }
            |> List.singleton
            |> a
                [ title titleStr
                , link
                    |> href
                , css [ Css.textDecoration Css.none ]
                ]

    else
        Nb.navbarProductItemStateSelectedWithInstances
            Nb.navbarProductItemStateSelectedAttributes
            Nb.navbarProductItemStateSelectedInstances
            { stateSelected = { iconInstance = img, productLabel = label, newLabelVisible = False } }


sidebarMenuItemWithNewParam : Html msg -> String -> String -> Bool -> String -> Bool -> Html msg
sidebarMenuItemWithNewParam img label titleStr selected link new =
    let
        ifNewAddEvenOdd =
            if new then
                [ Css.property "fill-rule" "evenodd"
                ]
                    |> css
                    |> List.singleton
                    |> Rs.s_subtract

            else
                identity
    in
    if not selected then
        Nb.navbarProductItemStateNeutralWithInstances
            (Nb.navbarProductItemStateNeutralAttributes
                |> Rs.s_pathfinder [ [ Css.hover Nb.navbarProductItemStateHoverPathfinder_details.styles ] |> css ]
                |> Rs.s_stateNeutral
                    [ [ Css.hover Nb.navbarProductItemStateHover_details.styles
                      ]
                        |> css
                    ]
                |> ifNewAddEvenOdd
            )
            Nb.navbarProductItemStateNeutralInstances
            { stateNeutral = { iconInstance = img, productLabel = label, newLabelVisible = new } }
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
                |> ifNewAddEvenOdd
            )
            Nb.navbarProductItemStateSelectedInstances
            { stateSelected = { iconInstance = img, productLabel = label, newLabelVisible = new } }


sidebar : Plugins -> Config -> Model key -> Html Msg
sidebar plugins vc model =
    let
        products =
            [ sidebarMenuItem (Nb.iconsPathfinder10 {}) "Pathfinder" "Pathfinder" (model.page == Graph) (model.graph.route |> Route.graphRoute |> Route.toUrl)
            , sidebarMenuItemWithNewParam (Nb.iconsPathfinder10 {}) "Pathfinder 2.0" "Pathfinder 2.0" (model.page == Pathfinder) (Route.pathfinderRoute Pathfinder.Root |> Route.toUrl) True
            ]
                ++ Plugin.sidebar plugins model.plugins model.page vc

        statsLinkItem =
            if model.page == Stats then
                Nb.textItremStateSelected
                    { stateSelected = { textLabel = Locale.string vc.locale "Statistics" } }

            else
                Nb.textItremStateNeutral
                    { stateNeutral = { textLabel = Locale.string vc.locale "Statistics" } }

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
            (if model.page == Settings then
                Nb.iconsSettingsLargeStateSelected {}

             else
                Nb.iconsSettingsLargeStateNeutral {}
            )
                |> List.singleton
                |> a
                    [ title (Locale.string vc.locale "Settings")
                    , (Route.settingsRoute |> Route.toUrl)
                        |> href
                    , Css.none |> Css.textDecoration |> List.singleton |> css
                    ]
    in
    Nb.navbarMenuNewWithInstances
        (Nb.navbarMenuNewAttributes
            |> Rs.s_navbarMenuNew
                (model.height |> toFloat |> Css.px |> Css.height |> List.singleton |> css |> List.singleton)
            |> Rs.s_iknaioLogo
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
        { navbarMenuNew =
            { helpLabel = ""

            -- , iconInstance = sidebarMenuItem (Nb.iconsSettingsLargeStateNeutral {}) "" (Locale.string vc.locale "Settings") (model.page == Settings) (Route.settingsRoute |> Route.toUrl)
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
                    Dialog.view vc dialog
                        |> ov (Dialog.defaultMsg dialog)

                Nothing ->
                    []
