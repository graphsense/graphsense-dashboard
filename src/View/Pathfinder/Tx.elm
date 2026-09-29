module View.Pathfinder.Tx exposing (edge, view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Model.Pathfinder.DetailLevel exposing (DetailLevel)
import Model.Pathfinder.Id as Id
import Model.Pathfinder.SearchBox exposing (Highlight, dimmedOpacity)
import Model.Pathfinder.Tx exposing (Tx, TxType(..))
import Msg.Pathfinder exposing (Msg)
import Svg.Styled exposing (Svg, g)
import Svg.Styled.Attributes exposing (css, opacity)
import Svg.Styled.Lazy as Svg
import Util.Annotations as Annotations
import View.Pathfinder.Tx.AccountTx as AccountTx
import View.Pathfinder.Tx.Utxo as Utxo exposing (RenderLevel)


view : View.Config -> Pathfinder.Config -> Highlight -> DetailLevel -> Tx -> Maybe Annotations.AnnotationItem -> Svg Msg
view vc gc searchHighlight level tx annotation =
    let
        inner =
            case tx.type_ of
                Utxo t ->
                    annotation
                        |> Utxo.view vc gc level tx t

                Account t ->
                    annotation
                        |> AccountTx.view vc gc level tx t

        attrs =
            dimmedOpacity searchHighlight ++ unservedAttrs vc tx
    in
    if List.isEmpty attrs then
        inner

    else
        g attrs [ inner ]


{-| A transaction of a network the backend no longer serves (lite networks
switched off in the settings) is drawn faded, like its addresses.
-}
unservedAttrs : View.Config -> Tx -> List (Svg.Styled.Attribute Msg)
unservedAttrs vc tx =
    if View.networkServed vc (Id.network tx.id) then
        []

    else
        [ opacity "0.3", css [ Css.property "filter" "grayscale(1)" ] ]


edge : View.Config -> Pathfinder.Config -> Highlight -> RenderLevel -> Tx -> Maybe Annotations.AnnotationItem -> Svg Msg
edge vc gc searchHighlight level tx annotation =
    let
        inner =
            case tx.type_ of
                Utxo t ->
                    Svg.lazy6 Utxo.edge vc gc level t tx annotation

                Account t ->
                    Svg.lazy5 AccountTx.edge vc gc t tx annotation

        attrs =
            dimmedOpacity searchHighlight ++ unservedAttrs vc tx
    in
    if List.isEmpty attrs then
        inner

    else
        g attrs [ inner ]
