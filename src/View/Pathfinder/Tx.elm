module View.Pathfinder.Tx exposing (edge, view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Model.Pathfinder.SearchBox exposing (Highlight, dimmedOpacity)
import Model.Pathfinder.Tx exposing (Tx, TxType(..))
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import Svg.Styled exposing (Svg, g)
import Svg.Styled.Lazy as Svg
import Util.Annotations as Annotations
import View.Pathfinder.Tx.AccountTx as AccountTx
import View.Pathfinder.Tx.Utxo as Utxo exposing (RenderLevel)


view : Plugins -> View.Config -> Pathfinder.Config -> Highlight -> Tx -> Maybe Annotations.AnnotationItem -> Svg Msg
view plugins vc gc searchHighlight tx annotation =
    let
        inner =
            case tx.type_ of
                Utxo t ->
                    annotation
                        |> Utxo.view plugins vc gc tx t

                Account t ->
                    annotation
                        |> AccountTx.view plugins vc gc tx t

        attrs =
            dimmedOpacity searchHighlight
    in
    if List.isEmpty attrs then
        inner

    else
        g attrs [ inner ]


edge : Plugins -> View.Config -> Pathfinder.Config -> Highlight -> RenderLevel -> Tx -> Maybe Annotations.AnnotationItem -> Svg Msg
edge plugins vc gc searchHighlight level tx annotation =
    let
        inner =
            case tx.type_ of
                Utxo t ->
                    Svg.lazy7 Utxo.edge plugins vc gc level t tx annotation

                Account t ->
                    Svg.lazy6 AccountTx.edge plugins vc gc t tx annotation

        attrs =
            dimmedOpacity searchHighlight
    in
    if List.isEmpty attrs then
        inner

    else
        g attrs [ inner ]
