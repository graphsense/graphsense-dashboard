module View.Pathfinder.Network exposing (ClusterContext, addresses, relations)

import Api.Data
import Basics.Extra exposing (uncurry)
import Config.Pathfinder as Pathfinder
import Config.View as View
import Dict exposing (Dict)
import List.Extra
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.SearchBox as SearchBox
import Model.Pathfinder.Tx exposing (Tx)
import Msg.Pathfinder exposing (Msg)
import Plugin.View exposing (Plugins)
import RemoteData exposing (WebData)
import Set
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Util.Annotations as Annotations
import View.Pathfinder.Address as Address
import View.Pathfinder.AggEdge as AggEdge
import View.Pathfinder.ConversionEdge as ConversionEdge
import View.Pathfinder.Tx as Tx
import View.Pathfinder.Tx.Utxo exposing (RenderLevel(..))


type alias ClusterContext =
    { clusters : Dict Id (WebData Api.Data.Cluster)
    , hoveredAddressId : Maybe Id
    , hoveredClusterId : Maybe Id
    }


addresses : Plugins -> View.Config -> Pathfinder.Config -> SearchBox.Model -> Annotations.AnnotationModel -> Dict Id Address -> Svg Msg
addresses plugins vc pc searchBox annotations =
    Dict.foldl
        (\id address svg ->
            ( Id.toString id
            , Annotations.getAnnotation id annotations
                |> Svg.lazy6 Address.view plugins vc pc (SearchBox.highlightFor searchBox id) address
            )
                :: svg
        )
        []
        >> Keyed.node "g" []


relations : Plugins -> View.Config -> Pathfinder.Config -> Bool -> Maybe Id -> SearchBox.Model -> Annotations.AnnotationModel -> Dict Id Tx -> Dict ( Id, Id ) AggEdge -> Dict ( Id, Id ) ConversionEdge -> Svg Msg
relations plugins vc gc showAggLabels focusAddressId searchBox annotations txs agg conversions =
    case gc.tracingMode of
        Pathfinder.AggregateTracingMode ->
            Svg.lazy5 aggRelations vc showAggLabels focusAddressId searchBox agg

        Pathfinder.TransactionTracingMode ->
            Svg.lazy7 txRelations plugins vc gc searchBox annotations txs conversions


{-| Render aggregate-mode edges.

  - `showAggLabels` — when False the always-on mid-point value labels are
    omitted to reduce clutter when zoomed out; hovered/selected edges still
    show their label via the highlight layer.
  - `focusAddressId` — the currently hovered address, if any. When an edge is
    hovered/selected or an address is hovered, edges not incident to that
    focus are dimmed so the relevant edges are easy to trace.

-}
aggRelations : View.Config -> Bool -> Maybe Id -> SearchBox.Model -> Dict ( Id, Id ) AggEdge -> Svg Msg
aggRelations vc showAggLabels focusAddressId searchBox agg =
    let
        agg_ =
            Dict.values agg
                |> List.filter
                    (\edge ->
                        RemoteData.isSuccess edge.a2b && RemoteData.isSuccess edge.b2a
                    )

        -- nodes touched by a hovered/selected edge, plus the hovered address
        focusedNodes =
            agg_
                |> List.concatMap
                    (\edge ->
                        if edge.hovered || edge.selected then
                            [ edge.a, edge.b ]

                        else
                            []
                    )
                |> (\ids ->
                        case focusAddressId of
                            Just fid ->
                                fid :: ids

                            Nothing ->
                                ids
                   )
                |> Set.fromList

        focusActive =
            not (Set.isEmpty focusedNodes)

        isDimmed edge =
            let
                searchDimmed =
                    SearchBox.highlightForAny searchBox [ edge.a, edge.b ] == SearchBox.Dimmed

                focusDimmed =
                    focusActive
                        && not (edge.hovered || edge.selected)
                        && not (Set.member edge.a focusedNodes)
                        && not (Set.member edge.b focusedNodes)
            in
            searchDimmed || focusDimmed
    in
    Svg.g []
        [ agg_
            |> List.filterMap
                (\edge ->
                    Maybe.map2 (aggEdgeEdge vc (isDimmed edge) edge)
                        edge.aAddress
                        edge.bAddress
                )
            |> Keyed.node "g" []
        , if showAggLabels then
            agg_
                |> List.filterMap
                    (\edge ->
                        Maybe.map2 (aggEdgeNode vc (isDimmed edge) edge)
                            edge.aAddress
                            edge.bAddress
                    )
                |> Keyed.node "g" []

          else
            Svg.g [] []
        , agg_
            |> List.filter (\a -> a.selected || a.hovered)
            |> List.filterMap
                (\edge ->
                    Maybe.map2 (aggEdgeNodeHighlight vc edge)
                        edge.aAddress
                        edge.bAddress
                )
            |> Keyed.node "g" []
        ]


txRelations : Plugins -> View.Config -> Pathfinder.Config -> SearchBox.Model -> Annotations.AnnotationModel -> Dict Id Tx -> Dict ( Id, Id ) ConversionEdge -> Svg Msg
txRelations plugins vc gc searchBox annotations txs conversions =
    let
        txs_ =
            Dict.values txs

        conversions_ =
            Dict.values conversions

        ( txsHighlighted_, txsRegular_ ) =
            List.partition (\tx -> tx.hovered || tx.selected) txs_
    in
    (if vc.showConversionEdges then
        [ conversions_
            |> List.filterMap
                (\x -> Maybe.map2 (\a b -> ( x, a, b )) x.inputAddress x.outputAddress)
            |> List.Extra.gatherEqualsBy (\( _, a, b ) -> ( a, b ))
            |> List.concatMap (uncurry (::) >> List.indexedMap Tuple.pair)
            |> List.map
                (\( i, ( conversion, a, b ) ) ->
                    conversionEdge plugins vc gc searchBox conversion i a b
                )
            |> Keyed.node "g" []
        ]

     else
        []
    )
        ++ [ txsRegular_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "te"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy7 Tx.edge plugins vc gc (SearchBox.highlightFor searchBox tx.id) Edge tx
                        )
                    )
                |> Keyed.node "g" []
           , txsRegular_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "tl"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy7 Tx.edge plugins vc gc (SearchBox.highlightFor searchBox tx.id) Label tx
                        )
                    )
                |> Keyed.node "g" []
           , txsRegular_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "tn"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy6 Tx.view plugins vc gc (SearchBox.highlightFor searchBox tx.id) tx
                        )
                    )
                |> Keyed.node "g" []
           , txsHighlighted_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "teh"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy7 Tx.edge plugins vc gc (SearchBox.highlightFor searchBox tx.id) Edge tx
                        )
                    )
                |> Keyed.node "g" []
           , txsHighlighted_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "tlh"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy7 Tx.edge plugins vc gc (SearchBox.highlightFor searchBox tx.id) Label tx
                        )
                    )
                |> Keyed.node "g" []
           , txsHighlighted_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "tnh"
                        , Annotations.getAnnotation tx.id annotations
                            |> Svg.lazy6 Tx.view plugins vc gc (SearchBox.highlightFor searchBox tx.id) tx
                        )
                    )
                |> Keyed.node "g" []
           ]
        |> Svg.g []


{-| Append a strong opacity reduction when an edge is dimmed for focus or
on-graph search.
-}
dimAttrs : Bool -> List (Svg.Attribute Msg)
dimAttrs dimmed =
    if dimmed then
        [ opacity "0.15" ]

    else
        []


aggEdgeNodeHighlight : View.Config -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeNodeHighlight vc edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "eh"
    , Svg.lazy4 AggEdge.highlight vc edge aAddress bAddress
    )


aggEdgeNode : View.Config -> Bool -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeNode vc dimmed edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "en"
    , Svg.g
        (dimAttrs dimmed)
        [ Svg.lazy4 AggEdge.view vc edge aAddress bAddress ]
    )


aggEdgeEdge : View.Config -> Bool -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeEdge vc dimmed edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "ee"
    , Svg.g
        (dimAttrs dimmed)
        [ Svg.lazy5 AggEdge.edge vc edge aAddress bAddress False ]
    )


conversionEdge : Plugins -> View.Config -> Pathfinder.Config -> SearchBox.Model -> ConversionEdge -> Int -> Address -> Address -> ( String, Svg Msg )
conversionEdge _ vc _ searchBox conversion displacementIndex aAddress bAddress =
    let
        ( inputTxId, outputTxId ) =
            conversion.id
    in
    ( ConversionEdge.toIdString conversion |> (++) "ce"
    , Svg.lazy6 ConversionEdge.view vc (SearchBox.highlightForAny searchBox [ aAddress.id, bAddress.id, inputTxId, outputTxId ]) conversion displacementIndex aAddress bAddress
    )



{-
   txs : Plugins -> View.Config -> Pathfinder.Config -> Annotations.AnnotationModel -> List Tx -> Svg Msg
   txs plugins vc gc annotations =
       List.foldl
           (\tx svg ->
               ( Id.bString tx.id
               , Annotations.getAnnotation tx.id annotations
                   |> Svg.lazy5 Tx.view plugins vc gc tx
               )
                   :: svg
           )
           []
           >> Keyed.node "g" []
-}
