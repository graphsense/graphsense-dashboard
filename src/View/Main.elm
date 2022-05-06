module View.Main exposing (main_)

import Graph.View.View as Graph
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (..)
import Page
import Stats.View as Stats
import View.Config exposing (Config)


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
