module View.Main exposing (view)

import Config.View as View
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (Model, Msg(..), Page(..))
import Plugin.View as Plugin
import RecordSetter as Rs
import Route
import Route.Pathfinder
import Theme.Html.Page as Page
import Util.View exposing (fullWidthCss)
import View.Landingpage as Landingpage
import View.Locale as Locale
import View.Pathfinder as Pathfinder
import View.Settings as Settings
import View.Stats as Stats


view :
    View.Config
    -> Model key
    -> Html Msg
view vc model =
    case model.page of
        Home ->
            [ Landingpage.view vc model ]
                |> main_ vc

        Stats ->
            Stats.stats vc model.stats model.supportedTokens

        Settings ->
            Settings.view vc model

        Pathfinder ->
            Pathfinder.view model.plugins vc model.pathfinder
                |> List.map (Html.Styled.map PathfinderMsg)
                |> main_ vc

        RetiredGraph ->
            [ retiredGraph vc ]
                |> main_ vc

        Plugin type_ ->
            Plugin.contents model.plugins type_ vc
                |> Maybe.map (main_ vc)
                |> Maybe.withDefault Util.View.none


retiredGraph : View.Config -> Html Msg
retiredGraph vc =
    Page.infoPageWithInstances
        (Page.infoPageAttributes
            |> Rs.s_root [ css [ fullWidthCss ] ]
        )
        (Page.infoPageInstances
            |> Rs.s_link
                (Just <|
                    a
                        [ css Page.infoPageLink_details.styles
                        , Route.Pathfinder.Root
                            |> Route.pathfinderRoute
                            |> Route.toUrl
                            |> href
                        ]
                        [ text <| Locale.string vc.locale "Open Pathfinder" ]
                )
        )
        { root =
            { title = Locale.string vc.locale "pf1_retired_title"
            , information = Locale.string vc.locale "pf1_retired_notice"
            , link = ""
            }
        }


main_ : View.Config -> List (Html Msg) -> Html Msg
main_ vc contents =
    Html.Styled.main_
        [ css
            [ Css.flexGrow (Css.num 1)
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.position Css.relative
            ]
        , id "contents"
        ]
        [ section
            [ [ Css.displayFlex
              , Css.flexDirection Css.column
              , Css.flexGrow (Css.num 1)
              , Css.overflow Css.auto
              ]
                ++ (vc.size
                        |> Maybe.map
                            (\{ height } ->
                                Css.px height
                                    |> Css.maxHeight
                                    |> List.singleton
                            )
                        |> Maybe.withDefault []
                   )
                |> css
            ]
            contents
        ]
