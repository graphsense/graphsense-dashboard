module View.Sidebar exposing (sidebarMenuItem, sidebarMenuItemWithNewParam, sidebarMenuItemWithSubMenu)

import Config.View exposing (Config)
import Css
import Html.Styled exposing (Html, a, div)
import Html.Styled.Attributes exposing (css, href, title)
import Model exposing (Model, Msg(..), NavbarSubMenu, NavbarSubMenuType(..))
import RecordSetter as Rs
import Theme.Colors
import Theme.Html.GraphComponents as GraphComponents
import Theme.Html.Icons as Icons
import Theme.Html.Navbar as Nb
import Util.Css
import Util.View exposing (fixFillRule, onClickWithStop)
import View.Locale as Locale
import View.Pathfinder.ContextMenuItem as ContextMenuItem


navbarSubMenuView : Config -> Model key -> NavbarSubMenu -> Html Msg
navbarSubMenuView vc _ { type_ } =
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
                GraphComponents.rightClickMenuWithAttributes
                    (GraphComponents.rightClickMenuAttributes
                        |> Rs.s_dividerLine [ [ Css.display Css.none ] |> css ]
                    )
                    { shortcutList =
                        []
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
