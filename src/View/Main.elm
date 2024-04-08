module View.Main exposing (view)

import Config.View as View
import Css.View as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (Model, Msg(..), Page(..))
import Plugin.View as Plugin exposing (Plugins)
import Util.View
import View.Graph as Graph
import View.Landingpage as Landingpage
import View.Pathfinder as Pathfinder
import View.Stats as Stats


view :
    Plugins
    -> View.Config
    -> Model key
    -> Html Msg
view plugins vc model =
    case model.page of
        Home ->
            { navbar = []
            , contents = [ Landingpage.view plugins vc model ]
            }
                |> main_ vc

        Stats ->
            Stats.stats vc model.stats model.supportedTokens

        Graph ->
            Graph.view plugins model.plugins vc model.graph
                |> (\{ navbar, contents } ->
                        { navbar = List.map (Html.Styled.map GraphMsg) navbar
                        , contents = List.map (Html.Styled.map GraphMsg) contents
                        }
                   )
                |> main_ vc

        Pathfinder ->
            Pathfinder.view plugins model.plugins vc model.pathfinder
                |> (\{ navbar, contents } ->
                        { navbar = List.map (Html.Styled.map PathfinderMsg) navbar
                        , contents = List.map (Html.Styled.map PathfinderMsg) contents
                        }
                   )
                |> main_ vc

        Plugin type_ ->
            Plugin.contents plugins model.plugins type_ vc
                |> Maybe.map
                    (\contents ->
                        main_ vc
                            { navbar =
                                Plugin.navbar plugins model.plugins type_ vc
                                    |> Maybe.withDefault []
                            , contents = contents
                            }
                    )
                |> Maybe.withDefault Util.View.none


main_ : View.Config -> { navbar : List (Html Msg), contents : List (Html Msg) } -> Html Msg
main_ vc { navbar, contents } =
    Html.Styled.main_
        [ Css.main_ vc |> css
        , id "contents"
        ]
        ((if List.isEmpty navbar then
            []

          else
            nav
                [ Css.navbar vc |> css
                ]
                navbar
                |> List.singleton
         )
            ++ [ section
                    [ Css.contents vc |> css
                    ]
                    contents
               ]
        )
