module View.Main exposing (main_)

import Config.View exposing (Config)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (Model, Msg(..), Page(..))
import Plugin.View as Plugin exposing (Plugins)
import Util.View
import View.Graph as Graph
import View.Stats as Stats


main_ :
    Plugins
    -> Config
    -> Model key
    -> Html Msg
main_ plugins vc model =
    case model.page of
        Stats ->
            Stats.stats vc model.stats

        Graph ->
            Graph.view plugins model.plugins vc model.graph
                |> Html.Styled.map GraphMsg

        Plugin type_ ->
            Plugin.main_ plugins model.plugins type_ vc
                |> Maybe.withDefault Util.View.none