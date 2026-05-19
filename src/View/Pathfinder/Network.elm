module View.Pathfinder.Network exposing (ClusterContext, addresses, groups, relations)

import Api.Data
import Basics.Extra exposing (uncurry)
import Color
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Dict exposing (Dict)
import Html.Styled.Attributes as HA
import Html.Styled.Events exposing (onDoubleClick)
import List.Extra
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.GroupBox as GroupBox
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.SearchBox as SearchBox
import Model.Pathfinder.Tx exposing (Tx)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.View exposing (Plugins)
import RemoteData exposing (WebData)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents
import Util.Annotations as Annotations
import Util.Graph
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
            , Annotations.getVisibleAnnotation id annotations
                |> Svg.lazy6 Address.view plugins vc pc (SearchBox.highlightFor searchBox id) address
            )
                :: svg
        )
        []
        >> Keyed.node "g" []


{-| Render one colored box per group, behind the nodes. The box bounds the
member nodes; its color/label come from the group's annotations. `draggedIds`
are the nodes currently being dragged — a dragged non-member inside a box is
previewed as captured (the box grows to include it).
-}
groups : Annotations.AnnotationModel -> List Id -> Dict Id Address -> Dict Id Tx -> Svg Msg
groups annotations draggedIds addresses_ txs =
    Annotations.groups annotations
        |> List.filterMap (groupBox draggedIds addresses_ txs)
        |> Keyed.node "g" []


groupBox : List Id -> Dict Id Address -> Dict Id Tx -> Annotations.Group -> Maybe ( String, Svg Msg )
groupBox draggedIds addresses_ txs group =
    GroupBox.boundsFor addresses_ txs group.members
        |> Maybe.map
            (\realBounds ->
                let
                    -- dragged non-members currently inside the box: previewed
                    -- as captured, so the box grows to enclose them
                    captured =
                        draggedIds
                            |> List.filter (\d -> not (List.member d group.members))
                            |> List.filter
                                (\d ->
                                    GroupBox.pointFor addresses_ txs d
                                        |> Maybe.map (GroupBox.contains realBounds)
                                        |> Maybe.withDefault False
                                )

                    b =
                        if List.isEmpty captured then
                            realBounds

                        else
                            GroupBox.boundsFor addresses_ txs (group.members ++ captured)
                                |> Maybe.withDefault realBounds

                    color =
                        group.color
                            |> Maybe.map Color.toCssString
                            |> Maybe.withDefault Colors.brandBlack
                in
                ( "group" ++ String.fromInt group.id
                  -- marks the box for inclusion in graph-export bounds
                , Svg.g [ HA.attribute "data-group" "true" ]
                    [ -- box body: non-interactive so the canvas stays pannable
                      rect
                        [ x (String.fromFloat b.minX)
                        , y (String.fromFloat b.minY)
                        , width (String.fromFloat (b.maxX - b.minX))
                        , height (String.fromFloat (b.maxY - b.minY))
                        , rx "8"
                        , stroke color
                        , strokeWidth "2"
                        , fill color
                        , fillOpacity "0.08"
                        , pointerEvents "none"
                        ]
                        []
                    , -- header strip: drag handle for the whole group
                      rect
                        [ x (String.fromFloat b.minX)
                        , y (String.fromFloat b.minY)
                        , width (String.fromFloat (b.maxX - b.minX))
                        , height (String.fromFloat GroupBox.headerHeight)
                        , rx "8"
                        , fill color
                        , fillOpacity "0.22"
                        , css [ Css.cursor Css.move ]
                        , onDoubleClick (UserDoubleClickedGroup group.members)
                        , Util.Graph.mousedown (UserPushesLeftMouseButtonOnGroup group.members)
                        ]
                        []
                    , if String.isEmpty group.label then
                        text ""

                      else
                        Svg.text_
                            [ x (String.fromFloat (b.minX + 8))
                            , y (String.fromFloat (b.minY + 15))
                            , css GraphComponents.annotationLabel2Label_details.styles
                            , fill color
                            , pointerEvents "none"
                            ]
                            [ text group.label ]
                    ]
                )
            )


relations : Plugins -> View.Config -> Pathfinder.Config -> SearchBox.Model -> Annotations.AnnotationModel -> Dict Id Tx -> Dict ( Id, Id ) AggEdge -> Dict ( Id, Id ) ConversionEdge -> Svg Msg
relations plugins vc gc searchBox annotations txs agg conversions =
    case gc.tracingMode of
        Pathfinder.AggregateTracingMode ->
            Svg.lazy6 aggRelations plugins vc gc searchBox annotations agg

        Pathfinder.TransactionTracingMode ->
            Svg.lazy7 txRelations plugins vc gc searchBox annotations txs conversions


aggRelations : Plugins -> View.Config -> Pathfinder.Config -> SearchBox.Model -> Annotations.AnnotationModel -> Dict ( Id, Id ) AggEdge -> Svg Msg
aggRelations plugins vc gc searchBox _ agg =
    let
        agg_ =
            Dict.values agg
                |> List.filter
                    (\edge ->
                        RemoteData.isSuccess edge.a2b && RemoteData.isSuccess edge.b2a
                    )
    in
    Svg.g []
        [ agg_
            |> List.filterMap
                (\edge ->
                    Maybe.map2 (aggEdgeEdge plugins vc gc searchBox edge)
                        edge.aAddress
                        edge.bAddress
                )
            |> Keyed.node "g" []
        , agg_
            |> List.filterMap
                (\edge ->
                    Maybe.map2 (aggEdgeNode plugins vc gc searchBox edge)
                        edge.aAddress
                        edge.bAddress
                )
            |> Keyed.node "g" []
        , agg_
            |> List.filter (\a -> a.selected || a.hovered)
            |> List.filterMap
                (\edge ->
                    Maybe.map2 (aggEdgeNodeHighlight plugins vc gc edge)
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
                        , Annotations.getVisibleAnnotation tx.id annotations
                            |> Svg.lazy7 Tx.edge plugins vc gc (SearchBox.highlightFor searchBox tx.id) Edge tx
                        )
                    )
                |> Keyed.node "g" []
           , txsRegular_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "tl"
                        , Annotations.getVisibleAnnotation tx.id annotations
                            |> Svg.lazy7 Tx.edge plugins vc gc (SearchBox.highlightFor searchBox tx.id) Label tx
                        )
                    )
                |> Keyed.node "g" []
           , txsRegular_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "tn"
                        , Annotations.getVisibleAnnotation tx.id annotations
                            |> Svg.lazy6 Tx.view plugins vc gc (SearchBox.highlightFor searchBox tx.id) tx
                        )
                    )
                |> Keyed.node "g" []
           , txsHighlighted_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "teh"
                        , Annotations.getVisibleAnnotation tx.id annotations
                            |> Svg.lazy7 Tx.edge plugins vc gc (SearchBox.highlightFor searchBox tx.id) Edge tx
                        )
                    )
                |> Keyed.node "g" []
           , txsHighlighted_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "tlh"
                        , Annotations.getVisibleAnnotation tx.id annotations
                            |> Svg.lazy7 Tx.edge plugins vc gc (SearchBox.highlightFor searchBox tx.id) Label tx
                        )
                    )
                |> Keyed.node "g" []
           , txsHighlighted_
                |> List.map
                    (\tx ->
                        ( Id.toString tx.id |> (++) "tnh"
                        , Annotations.getVisibleAnnotation tx.id annotations
                            |> Svg.lazy6 Tx.view plugins vc gc (SearchBox.highlightFor searchBox tx.id) tx
                        )
                    )
                |> Keyed.node "g" []
           ]
        |> Svg.g []


aggEdgeNodeHighlight : Plugins -> View.Config -> Pathfinder.Config -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeNodeHighlight _ vc _ edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "eh"
    , Svg.lazy4 AggEdge.highlight vc edge aAddress bAddress
    )


aggEdgeNode : Plugins -> View.Config -> Pathfinder.Config -> SearchBox.Model -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeNode _ vc _ searchBox edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "en"
    , Svg.g
        (SearchBox.dimmedOpacity (SearchBox.highlightForAny searchBox [ aAddress.id, bAddress.id ]))
        [ Svg.lazy4 AggEdge.view vc edge aAddress bAddress ]
    )


aggEdgeEdge : Plugins -> View.Config -> Pathfinder.Config -> SearchBox.Model -> AggEdge -> Address -> Address -> ( String, Svg Msg )
aggEdgeEdge _ vc _ searchBox edge aAddress bAddress =
    ( Id.toString edge.a ++ Id.toString edge.b |> (++) "ee"
    , Svg.g
        (SearchBox.dimmedOpacity (SearchBox.highlightForAny searchBox [ aAddress.id, bAddress.id ]))
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
               , Annotations.getVisibleAnnotation tx.id annotations
                   |> Svg.lazy5 Tx.view plugins vc gc tx
               )
                   :: svg
           )
           []
           >> Keyed.node "g" []
-}
