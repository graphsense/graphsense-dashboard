module Scenario.CrosschainHoverTest exposing (suite)

{-| Hovering an address marks the nodes that are the same address on another
chain (their cross-chain icon glows), so a graph that holds one EVM address on
eth and on arb shows they belong together. Leaving the node clears the mark.
-}

import Api.Data
import Dict
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Html.Attributes
import Json.Decode
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Route.Pathfinder as Route
import Set
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Update.Pathfinder.Network


evm : String
evm =
    "0x4d5d0da4848cf76ca3f916c7c8b2c772cd47f540"


ethNode : Id
ethNode =
    ( "eth", evm )


arbNode : Id
arbNode =
    ( "arb", evm )


btcNode : Id
btcNode =
    ( "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" )


answerAddresses : App -> App
answerAddresses =
    App.respond
        (\eff ->
            case eff of
                Effect.Api.GetAddressEffect params toMsg ->
                    Json.Decode.decodeString Api.Data.addressDecoder Fixture.address
                        |> Result.toMaybe
                        |> Maybe.map (\a -> toMsg { a | currency = params.currency, address = params.address })

                _ ->
                    Nothing
        )


open : Id -> App -> App
open ( network, address ) =
    App.navigate (Route.addressRoute { network = network, address = address })
        >> answerAddresses


{-| eth, then the same address on arb, then a btc address, with the details
panel closed: hovering the address whose details are open shows no hover
effects.
-}
graph : App
graph =
    Route.addressRoute { network = "eth", address = evm }
        |> App.initAt
        |> answerAddresses
        |> open arbNode
        |> open btcNode
        |> App.step UserClosedDetailsView


marked : Id -> App -> Maybe Bool
marked id app =
    Dict.get id (App.model app).network.addresses
        |> Maybe.map .crosschainSiblingHovered


renderedMarks : Int -> App -> Expectation
renderedMarks n app =
    App.html app
        |> Query.findAll [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-crosschain-sibling-mark") ]
        |> Query.count (Expect.equal n)


{-| What the related-addresses lookup fills in: the chains the address is
known on besides its own, which is what puts the cross-chain icon on a node.
-}
listsTwins : Id -> List ( String, String ) -> App -> App
listsTwins node twins =
    App.mapModel
        (\m ->
            { m
                | network =
                    Update.Pathfinder.Network.updateAddress node
                        (\a ->
                            { a
                                | networks =
                                    List.foldl (\( net, addr ) -> Dict.update net (Maybe.withDefault Set.empty >> Set.insert addr >> Just))
                                        a.networks
                                        twins
                            }
                        )
                        m.network
            }
        )


suite : Test
suite =
    describe "hovering an address on the graph"
        [ test "all three addresses are on the graph" <|
            \_ ->
                List.map (\id -> marked id graph) [ ethNode, arbNode, btcNode ]
                    |> Expect.equal [ Just False, Just False, Just False ]
        , test "marks the same address on another chain, and only that one" <|
            \_ ->
                App.step (UserMovesMouseOverAddress ethNode) graph
                    |> (\a -> List.map (\id -> marked id a) [ ethNode, arbNode, btcNode ])
                    |> Expect.equal [ Just False, Just True, Just False ]
        , test "the twin's cross-chain icon glows" <|
            \_ ->
                graph
                    |> listsTwins arbNode [ ( "arb", evm ), ( "eth", evm ) ]
                    |> App.step (UserMovesMouseOverAddress ethNode)
                    |> renderedMarks 1
        , test "a twin without a cross-chain icon gets none on hover" <|
            \_ ->
                -- its own lookup lists no twin (the address is unused on the
                -- other chain): a glowing icon appearing would claim one
                App.step (UserMovesMouseOverAddress ethNode) graph
                    |> renderedMarks 0
        , test "works the other way round" <|
            \_ ->
                App.step (UserMovesMouseOverAddress arbNode) graph
                    |> marked ethNode
                    |> Expect.equal (Just True)
        , test "an address with no twin on the graph marks nothing" <|
            \_ ->
                App.step (UserMovesMouseOverAddress btcNode) graph
                    |> (\a -> List.map (\id -> marked id a) [ ethNode, arbNode ])
                    |> Expect.equal [ Just False, Just False ]
        , test "moving from one twin to an unrelated node clears the mark" <|
            \_ ->
                graph
                    |> App.step (UserMovesMouseOverAddress ethNode)
                    |> App.step (UserMovesMouseOverAddress btcNode)
                    |> marked arbNode
                    |> Expect.equal (Just False)
        , test "an address the related-addresses lookup names is marked too" <|
            \_ ->
                -- a TRON address derived from the same key as an eth one has
                -- no common string; the lookup fills `networks` on one side
                graph
                    |> listsTwins btcNode [ ( "eth", evm ) ]
                    |> App.step (UserMovesMouseOverAddress ethNode)
                    |> (\a -> List.map (\id -> marked id a) [ arbNode, btcNode ])
                    |> Expect.equal [ Just True, Just True ]
        , test "leaving the node clears the mark" <|
            \_ ->
                graph
                    |> App.step (UserMovesMouseOverAddress ethNode)
                    |> App.step (UserMovesMouseOutAddress ethNode)
                    |> marked arbNode
                    |> Expect.equal (Just False)
        ]
