module Encode.Pathfinder exposing (encode, encodeSelection)

import Animation
import Color exposing (Color)
import Dict
import Json.Encode exposing (Value, bool, float, int, list, null, string)
import Model.Pathfinder exposing (Model)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..))
import Model.Pathfinder.Tx exposing (Tx)
import Set
import Util.Annotations exposing (AnnotationItem, toList)


encode : Model -> Value
encode =
    encodeWith { keepAddress = always True, keepTx = always True }


{-| The same file format, restricted to a multi-selection: the selected nodes,
their annotations, and the agg edges whose both ends are selected. Taken
literally -- a selected tx whose addresses are not selected is written without
them, and the loader then shows it with its address slots empty, as it does for
any tx whose addresses are not on the graph.
-}
encodeSelection : List MultiSelectOptions -> Model -> Value
encodeSelection selection =
    let
        ( addresses, txs ) =
            selection
                |> List.foldl
                    (\sel ( as_, ts ) ->
                        case sel of
                            MSelectedAddress id ->
                                ( Set.insert id as_, ts )

                            MSelectedTx id ->
                                ( as_, Set.insert id ts )
                    )
                    ( Set.empty, Set.empty )
    in
    encodeWith
        { keepAddress = \id -> Set.member id addresses
        , keepTx = \id -> Set.member id txs
        }


encodeWith : { keepAddress : Id -> Bool, keepTx : Id -> Bool } -> Model -> Value
encodeWith { keepAddress, keepTx } model =
    [ string "pathfinder"
    , string "1"
    , string model.name
    , model.network.addresses
        |> Dict.values
        |> List.filter (.id >> keepAddress)
        |> list encodeAddress
    , model.network.txs
        |> Dict.values
        |> List.filter (.id >> keepTx)
        |> list encodeTx
    , model.annotations
        |> toList
        |> List.filter (\( id, _ ) -> keepAddress id || keepTx id)
        |> list encodeAnnotation
    , model.network.aggEdges
        |> Dict.values
        |> List.filter (\edge -> keepAddress edge.a && keepAddress edge.b)
        |> list encodeAggEdge
    ]
        |> list identity


encodeAggEdge : AggEdge -> Value
encodeAggEdge edge =
    [ encodeId edge.a
    , encodeId edge.b
    , edge.txs
        |> Set.toList
        |> list encodeId
    , edge.labelOffset
        |> Maybe.map (\o -> list float [ o.x, o.y ])
        |> Maybe.withDefault null
    ]
        |> list identity


encodeId : Id -> Value
encodeId id =
    [ Id.network id
    , Id.id id
    ]
        |> list string


encodeAddress : Address -> Value
encodeAddress address =
    [ encodeId address.id
    , float address.x
    , float (Animation.getTo address.y)
    , bool address.isStartingPoint
    ]
        |> list identity


encodeTx : Tx -> Value
encodeTx tx =
    [ encodeId tx.id
    , float tx.x
    , float (Animation.getTo tx.y)
    , bool tx.isStartingPoint
    , int tx.index
    ]
        |> list identity


encodeAnnotation : ( Id, AnnotationItem ) -> Value
encodeAnnotation ( id, annotation ) =
    [ id |> encodeId
    , annotation.label |> string
    , annotation.color |> encodeColor
    ]
        |> list identity


encodeColor : Maybe Color -> Value
encodeColor =
    Maybe.map Color.toRgba
        >> Maybe.map
            (\c ->
                [ c.red
                , c.green
                , c.blue
                , c.alpha
                ]
                    |> list float
            )
        >> Maybe.withDefault null
