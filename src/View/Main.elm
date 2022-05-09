module View.Main exposing (main_)

import Config.View exposing (Config)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (Model, Msg(..))
import Page
import View.Graph as Graph
import View.Stats as Stats


main_ :
    Config
    -> Model key
    -> Html Msg
main_ vc model =
    case model.page of
        Page.Stats ->
            Stats.stats vc model.stats

        Page.Graph ->
            Graph.view vc model.graph
                |> Html.Styled.map GraphMsg
