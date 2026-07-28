module Serialization.RoundTripTest exposing (suite)

{-| `.gs` files are user data: a broken save or a broken open loses work that
cannot be reconstructed. `Encode.Pathfinder.encode` and
`Update.Pathfinder.deserialize` / `fromDeserialized` are written independently
of each other, so only a test keeps the two halves of the format in sync.

Two levels are covered here:

  - **whole model** — save a graph, open it in a _fresh_ session, save again:
    the two files must be byte-identical. Only for graphs whose contents are
    restored synchronously (addresses, annotations, agg edges).

  - **wire format** — for transactions, `fromDeserialized` only re-fetches them
    from the API, so the model is deliberately incomplete until the response
    lands. There the assertion is that the decoded payload carries everything
    the encoder wrote, and that the bulk request is actually emitted.

The "legacy files" group pins the tolerant decoder paths that let today's build
open files written by older ones.

-}

import Animation
import Color
import Data.Pathfinder.Id as Id
import Data.Pathfinder.Network as Network
import Dict
import Effect.Api
import Effect.Pathfinder
import Encode.Pathfinder
import Expect
import Init.Pathfinder
import Init.Pathfinder.AggEdge as AggEdge
import Json.Decode
import Json.Encode
import Model.Pathfinder exposing (Model)
import Model.Pathfinder.Deserialize exposing (Deserialized)
import Model.Pathfinder.Network exposing (Network)
import Plugin.Update as Plugin
import RecordSetter exposing (s_annotations, s_index, s_labelOffset, s_name, s_network, s_txs)
import Set
import Test exposing (Test, describe, test)
import Update.Pathfinder
import Util.Annotations as Annotations



-- MODELS


emptyModel : Model
emptyModel =
    Init.Pathfinder.init
        { snapToGrid = Nothing
        , highlightClusterFriends = Nothing
        , tracingMode = Nothing
        , avoidOverlapingNodes = Nothing
        , recentSearches = []
        }
        |> Tuple.first


withNetwork : Network -> Model
withNetwork network =
    emptyModel |> s_network network


withAggEdge : Network -> Network
withAggEdge network =
    let
        edge =
            AggEdge.init Id.address1 Id.address2
                |> s_txs (Set.fromList [ Id.tx1, Id.tx2 ])
                |> s_labelOffset (Just { x = 3.5, y = -4.25 })
    in
    { network
        | aggEdges =
            Dict.insert (AggEdge.initId Id.address1 Id.address2) edge network.aggEdges
    }


{-| One graph exercising every part of the format at once: a starting point and
a plain address, a transaction with a non-zero stacking index, an agg edge with
transactions and a pinned label offset, and annotations with and without a
colour.
-}
goldenModel : Model
goldenModel =
    let
        network =
            Network.twoConnectedAddresses
    in
    { network
        | txs = Dict.map (\_ tx -> s_index 7 tx) network.txs
        , aggEdges =
            Dict.insert (AggEdge.initId Id.address1 Id.address3)
                (AggEdge.init Id.address1 Id.address3
                    |> s_txs (Set.singleton Id.tx1)
                    |> s_labelOffset (Just { x = 3.5, y = -4.25 })
                )
                network.aggEdges
    }
        |> withNetwork
        |> s_name "golden"
        |> s_annotations
            (Annotations.empty
                |> Annotations.set Id.address1 "suspicious" (Just Color.red)
                |> Annotations.set Id.address3 "plain" Nothing
            )


{-| The exact bytes `goldenModel` must serialize to. Every released build has to
keep reading files in this shape, so a diff here is a compatibility decision,
not a formatting detail: update it only together with a decoder that still
accepts the previous layout (see "legacy files" below).
-}
goldenFile : String
goldenFile =
    """["pathfinder","1","golden",[[["btc","a1234567"],0,0,true],[["btc","a3456789"],8,0,false]],[[["btc","d1234567"],4,0,false,7]],[[["btc","a1234567"],"suspicious",[0.8,0,0,1]],[["btc","a3456789"],"plain",null]],[[["btc","a1234567"],["btc","a3456789"],[["btc","d1234567"]],[3.5,-4.25]]]]"""


annotated : Model -> Model
annotated model =
    Annotations.empty
        |> Annotations.set Id.address1 "suspicious" (Just Color.red)
        |> Annotations.set Id.address2 "no colour here" Nothing
        |> (\annotations -> s_annotations annotations model)



-- ROUND TRIPS


{-| Save, then open in a fresh session — what the user actually does.
-}
reopen : Model -> Result Json.Decode.Error ( Model, List Effect.Pathfinder.Effect )
reopen model =
    Encode.Pathfinder.encode model
        |> Update.Pathfinder.deserialize
        |> Result.map (\d -> Update.Pathfinder.fromDeserialized Plugin.empty d emptyModel)


expectStableFile : Model -> Expect.Expectation
expectStableFile model =
    reopen model
        |> Result.map (Tuple.first >> Encode.Pathfinder.encode >> Json.Encode.encode 0)
        |> Expect.equal (Ok (Encode.Pathfinder.encode model |> Json.Encode.encode 0))


decoded : Model -> Result Json.Decode.Error Deserialized
decoded =
    Encode.Pathfinder.encode >> Update.Pathfinder.deserialize


isBulkTxRequest : Effect.Pathfinder.Effect -> Bool
isBulkTxRequest eff =
    case eff of
        Effect.Pathfinder.ApiEffect (Effect.Api.BulkGetTxEffect _ _) ->
            True

        _ ->
            False



-- TESTS


suite : Test
suite =
    describe "Pathfinder .gs serialization"
        [ describe "the file format itself"
            [ test "a full graph serializes to exactly the documented bytes" <|
                \_ ->
                    -- Pinning the bytes is what makes the round-trip tests below
                    -- meaningful: on their own they compare the encoder against
                    -- itself, so a field dropped on *both* sides goes unnoticed.
                    Encode.Pathfinder.encode goldenModel
                        |> Json.Encode.encode 0
                        |> Expect.equal goldenFile
            , test "and that exact file still opens" <|
                \_ ->
                    goldenFile
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.andThen Update.Pathfinder.deserialize
                        |> Result.map
                            (\d ->
                                { name = d.name
                                , addresses = List.map .id d.addresses
                                , txs = List.map (\t -> ( t.id, t.index )) d.txs
                                , annotations = List.map (\a -> ( a.id, a.label, a.color )) d.annotations
                                , aggEdges = List.map (\e -> ( e.a, e.b, e.labelOffset )) d.aggEdges
                                }
                            )
                        |> Expect.equal
                            (Ok
                                { name = "golden"
                                , addresses = [ Id.address1, Id.address3 ]
                                , txs = [ ( Id.tx1, 7 ) ]
                                , annotations =
                                    [ ( Id.address1, "suspicious", Just (Color.rgba 0.8 0 0 1) )
                                    , ( Id.address3, "plain", Nothing )
                                    ]
                                , aggEdges = [ ( Id.address1, Id.address3, Just { x = 3.5, y = -4.25 } ) ]
                                }
                            )
            ]
        , describe "a saved graph reopens unchanged"
            [ test "empty graph" <|
                \_ -> expectStableFile emptyModel
            , test "single address" <|
                \_ -> expectStableFile (withNetwork Network.oneAddress)
            , test "several addresses" <|
                \_ -> expectStableFile (withNetwork Network.twoIndependentAddresses)
            , test "addresses with an agg edge" <|
                \_ ->
                    Network.twoIndependentAddresses
                        |> withAggEdge
                        |> withNetwork
                        |> expectStableFile
            , test "annotations, with and without a colour" <|
                \_ ->
                    Network.twoIndependentAddresses
                        |> withNetwork
                        |> annotated
                        |> expectStableFile
            , test "the graph name" <|
                \_ ->
                    withNetwork Network.oneAddress
                        |> s_name "Case 4711 — exchange payout"
                        |> expectStableFile
            , test "the starting point flag" <|
                \_ ->
                    -- address1 is the starting point in the fixture network;
                    -- losing the flag changes how the graph is laid out on open
                    reopen (withNetwork Network.oneAddress)
                        |> Result.map
                            (Tuple.first
                                >> .network
                                >> .addresses
                                >> Dict.get Id.address1
                                >> Maybe.map .isStartingPoint
                            )
                        |> Expect.equal (Ok (Just True))
            ]
        , describe "transactions"
            [ test "are carried by the file" <|
                \_ ->
                    decoded (withNetwork Network.twoConnectedAddresses)
                        |> Result.map (.txs >> List.map .id)
                        |> Expect.equal (Ok [ Id.tx1 ])
            , test "keep their coordinates and stacking index" <|
                \_ ->
                    -- every fixture transaction sits at index 0, so bump one:
                    -- otherwise an encoder that drops the index entirely still
                    -- passes (the decoder defaults it to 0)
                    let
                        network =
                            Network.twoConnectedAddresses
                                |> (\nw ->
                                        { nw
                                            | txs =
                                                Dict.map (\_ tx -> s_index 7 tx) nw.txs
                                        }
                                   )
                    in
                    decoded (withNetwork network)
                        |> Result.map (.txs >> List.map (\t -> ( t.x, t.y, t.index )))
                        |> Expect.equal
                            (network.txs
                                |> Dict.values
                                |> List.map (\t -> ( t.x, Animation.getTo t.y, t.index ))
                                |> Ok
                            )
            , test "are re-fetched instead of restored from the file" <|
                \_ ->
                    -- the file only stores ids and positions, so opening a graph
                    -- with transactions must emit the bulk request that fills in
                    -- the rest — without it the transactions never appear
                    reopen (withNetwork Network.twoConnectedAddresses)
                        |> Result.map (Tuple.second >> List.any isBulkTxRequest)
                        |> Expect.equal (Ok True)
            ]
        , describe "addresses of every fixture graph survive"
            ([ ( "one address with an outgoing tx", Network.oneAddressWithOutgoingTx )
             , ( "one address with two txs", Network.oneAddressWithTwoTxs )
             , ( "two connected addresses", Network.twoConnectedAddresses )
             , ( "one to two addresses", Network.one2TwoAddresses )
             , ( "one to three addresses", Network.one2ThreeAddresses )
             , ( "two txs to three addresses", Network.one2TwoTxs2ThreeAddresses )
             ]
                |> List.map
                    (\( name, network ) ->
                        test name <|
                            \_ ->
                                decoded (withNetwork network)
                                    |> Result.map (.addresses >> List.map .id)
                                    |> Expect.equal (Ok (Dict.keys network.addresses))
                    )
            )
        , describe "legacy files still open"
            [ test "a file written before agg edges existed" <|
                \_ ->
                    -- element 6 is missing entirely
                    """["pathfinder","1","old graph",[[["btc","a1"],1,2,true]],[],[]]"""
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.andThen Update.Pathfinder.deserialize
                        |> Result.map (\d -> ( d.name, List.length d.addresses, d.aggEdges ))
                        |> Expect.equal (Ok ( "old graph", 1, [] ))
            , test "an agg edge saved before labelOffset existed" <|
                \_ ->
                    """["pathfinder","1","g",[],[],[],[[["btc","a1"],["btc","a2"],[]]]]"""
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.andThen Update.Pathfinder.deserialize
                        |> Result.map (.aggEdges >> List.map .labelOffset)
                        |> Expect.equal (Ok [ Nothing ])
            , test "an agg edge saved with the vertical-only labelOffset" <|
                \_ ->
                    """["pathfinder","1","g",[],[],[],[[["btc","a1"],["btc","a2"],[],7.5]]]"""
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.andThen Update.Pathfinder.deserialize
                        |> Result.map (.aggEdges >> List.map .labelOffset)
                        |> Expect.equal (Ok [ Just { x = 0, y = 7.5 } ])
            , test "a thing saved before the stacking index existed" <|
                \_ ->
                    """["pathfinder","1","g",[],[[["btc","t1"],1,2,false]],[],[]]"""
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.andThen Update.Pathfinder.deserialize
                        |> Result.map (.txs >> List.map .index)
                        |> Expect.equal (Ok [ 0 ])
            , test "eth ids are normalized to lower case on open" <|
                \_ ->
                    -- files saved with checksummed eth addresses must still match
                    -- the ids the API returns
                    """["pathfinder","1","g",[[["eth","0xFB50526F49894B78541B776F5AAEFE43E3BD8590"],0,0,false]],[],[],[]]"""
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.andThen Update.Pathfinder.deserialize
                        |> Result.map (.addresses >> List.map .id)
                        |> Expect.equal (Ok [ ( "eth", "0xfb50526f49894b78541b776f5aaefe43e3bd8590" ) ])
            ]
        ]
