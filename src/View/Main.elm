module View.Main exposing (view)

import Config.View as View
import Css
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (Model, Msg(..), Page(..))
import Plugin.View as Plugin
import Route
import Route.Pathfinder
import Util.View
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
            { navbar = []
            , contents = [ Landingpage.view vc model ]
            }
                |> main_ vc

        Stats ->
            Stats.stats vc model.stats model.supportedTokens

        Settings ->
            Settings.view vc model

        Pathfinder ->
            Pathfinder.view model.plugins vc model.pathfinder
                |> (\{ navbar, contents } ->
                        { navbar = List.map (Html.Styled.map PathfinderMsg) navbar
                        , contents = List.map (Html.Styled.map PathfinderMsg) contents
                        }
                   )
                |> main_ vc

        RetiredGraph ->
            { navbar = []
            , contents = [ retiredGraph vc ]
            }
                |> main_ vc

        Plugin type_ ->
            Plugin.contents model.plugins type_ vc
                |> Maybe.map
                    (\contents ->
                        main_ vc
                            { navbar =
                                Plugin.navbar model.plugins type_ vc
                                    |> Maybe.withDefault []
                            , contents = contents
                            }
                    )
                |> Maybe.withDefault Util.View.none


retiredGraph : View.Config -> Html Msg
retiredGraph vc =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.flexGrow (Css.num 1)
            , Css.textAlign Css.center
            , Css.padding (Css.px 50)
            ]
        ]
        [ h2
            [ Css.View.heading2 vc |> css ]
            [ Locale.text vc.locale "pf1_retired_title" ]
        , p
            [ Css.View.paragraph vc |> css
            , css [ Css.maxWidth (Css.px 600) ]
            ]
            [ Locale.text vc.locale "pf1_retired_notice" ]
        , a
            [ Css.View.link vc |> css
            , Route.Pathfinder.Root
                |> Route.pathfinderRoute
                |> Route.toUrl
                |> href
            ]
            [ Locale.text vc.locale "Open Pathfinder" ]
        ]


main_ : View.Config -> { navbar : List (Html Msg), contents : List (Html Msg) } -> Html Msg
main_ vc { navbar, contents } =
    Html.Styled.main_
        [ Css.View.main_ vc |> css
        , id "contents"
        ]
        ((if List.isEmpty navbar then
            []

          else
            nav
                [ Css.View.navbar vc |> css
                ]
                navbar
                |> List.singleton
         )
            ++ [ section
                    [ Css.View.contents vc |> css
                    ]
                    contents
               ]
        )
