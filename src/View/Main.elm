module View.Main exposing (view)

import Config.View as View
import Css.View as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (Model, Msg(..), Page(..))
import Plugin.View as Plugin exposing (Plugins)
import Util.View
import View.Graph as Graph
import View.Graph.Navbar
import View.Stats as Stats


view :
    Plugins
    -> View.Config
    -> Model key
    -> Html Msg
view plugins vc model =
    case model.page of
        Stats ->
            Stats.stats vc model.stats

        Graph ->
            Graph.view plugins model.plugins vc model.graph
                |> (\{ navbar, contents } ->
                        { navbar = List.map (Html.Styled.map GraphMsg) navbar
                        , contents = List.map (Html.Styled.map GraphMsg) contents
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
    div
        [ Css.main_ vc |> css
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
                    , id "contents"
                    ]
                    contents
               ]
        )
