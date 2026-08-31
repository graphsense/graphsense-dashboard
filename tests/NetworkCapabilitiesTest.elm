module NetworkCapabilitiesTest exposing (suite)

import Api.Data
import Expect
import Model.NetworkCapabilities as NC
import Test exposing (Test, describe, test)


capabilities : List ( String, List String ) -> Api.Data.Capabilities
capabilities networks =
    { networks =
        networks
            |> List.map (\( network, disabled ) -> { network = network, disabled = disabled })
    }


suite : Test
suite =
    describe "NetworkCapabilities"
        [ test "before any response every network is fully enabled" <|
            \_ ->
                NC.none
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Relations caps "arb" |> Expect.equal True
                        , \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal True
                        ]
        , test "network absent from the response is fully enabled" <|
            \_ ->
                NC.fromApi (capabilities [ ( "arb", [ "relations" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "eth" |> Expect.equal False
                        , \caps -> NC.supports NC.Relations caps "eth" |> Expect.equal True
                        , \caps -> NC.supports NC.Tags caps "eth" |> Expect.equal True
                        , \caps -> NC.supports NC.Conversions caps "eth" |> Expect.equal True
                        ]
        , test "disabled features are not supported, the rest are, network is limited" <|
            \_ ->
                NC.fromApi (capabilities [ ( "arb", [ "relations", "clusters", "conversions" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.supports NC.Relations caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Clusters caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Conversions caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal True
                        , \caps -> NC.isLimitedNetwork caps "arb" |> Expect.equal True
                        ]
        , test "empty disabled list = fully enabled network" <|
            \_ ->
                NC.fromApi (capabilities [ ( "eth", [] ) ])
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "eth" |> Expect.equal False
                        , \caps -> NC.supports NC.Relations caps "eth" |> Expect.equal True
                        ]
        , test "unknown vocabulary words are tolerated and still mark the network as limited" <|
            \_ ->
                NC.fromApi (capabilities [ ( "arb", [ "teleportation", "relations" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.supports NC.Relations caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal True
                        , \caps -> NC.isLimitedNetwork caps "arb" |> Expect.equal True
                        ]
        , test "exact_stats is part of the vocabulary" <|
            \_ ->
                NC.fromApi (capabilities [ ( "arb", [ "exact_stats" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.supports NC.ExactStats caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.ExactStats caps "eth" |> Expect.equal True
                        , \caps -> NC.isLimitedNetwork caps "arb" |> Expect.equal True
                        ]
        , test "matching is case-insensitive on network and capability" <|
            \_ ->
                NC.fromApi (capabilities [ ( "ARB", [ "Tags" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal False
                        , \caps -> NC.isLimitedNetwork caps "Arb" |> Expect.equal True
                        ]
        ]
