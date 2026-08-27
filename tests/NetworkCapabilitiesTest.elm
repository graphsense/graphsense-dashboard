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
        [ test "network absent from the response = full core network" <|
            \_ ->
                NC.fromBuildConfig []
                    |> NC.withCapabilities (capabilities [ ( "arb", [ "relations" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "eth" |> Expect.equal False
                        , \caps -> NC.supports NC.Relations caps "eth" |> Expect.equal True
                        , \caps -> NC.supports NC.Tags caps "eth" |> Expect.equal True
                        , \caps -> NC.supports NC.Conversions caps "eth" |> Expect.equal True
                        ]
        , test "disabled features are not supported, the rest are, network is lite" <|
            \_ ->
                NC.fromBuildConfig []
                    |> NC.withCapabilities (capabilities [ ( "arb", [ "relations", "clusters", "conversions" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.supports NC.Relations caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Clusters caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Conversions caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal True
                        , \caps -> NC.isLimitedNetwork caps "arb" |> Expect.equal True
                        ]
        , test "empty disabled list = fully enabled core network" <|
            \_ ->
                NC.fromBuildConfig []
                    |> NC.withCapabilities (capabilities [ ( "eth", [] ) ])
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "eth" |> Expect.equal False
                        , \caps -> NC.supports NC.Relations caps "eth" |> Expect.equal True
                        ]
        , test "unknown vocabulary words are tolerated" <|
            \_ ->
                NC.fromBuildConfig []
                    |> NC.withCapabilities (capabilities [ ( "arb", [ "teleportation", "relations" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.supports NC.Relations caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal True
                        , \caps -> NC.isLimitedNetwork caps "arb" |> Expect.equal True
                        ]
        , test "matching is case-insensitive on network and capability" <|
            \_ ->
                NC.fromBuildConfig []
                    |> NC.withCapabilities (capabilities [ ( "ARB", [ "Tags" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal False
                        , \caps -> NC.isLimitedNetwork caps "Arb" |> Expect.equal True
                        ]
        , test "build-config seed disables everything before /capabilities arrives" <|
            \_ ->
                NC.fromBuildConfig [ "bnb", "arb" ]
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "bnb" |> Expect.equal True
                        , \caps -> NC.supports NC.Relations caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Clusters caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal False
                        , \caps -> NC.supports NC.Conversions caps "arb" |> Expect.equal False
                        , \caps -> NC.isLimitedNetwork caps "eth" |> Expect.equal False
                        ]
        , test "a /capabilities declaration overrides the build-config seed" <|
            \_ ->
                NC.fromBuildConfig [ "arb" ]
                    |> NC.withCapabilities (capabilities [ ( "arb", [ "relations" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.supports NC.Tags caps "arb" |> Expect.equal True
                        , \caps -> NC.supports NC.Relations caps "arb" |> Expect.equal False
                        ]
        , test "a seeded network absent from the response becomes fully enabled" <|
            \_ ->
                NC.fromBuildConfig [ "bnb" ]
                    |> NC.withCapabilities (capabilities [ ( "arb", [ "relations" ] ) ])
                    |> Expect.all
                        [ \caps -> NC.isLimitedNetwork caps "bnb" |> Expect.equal False
                        , \caps -> NC.supports NC.Relations caps "bnb" |> Expect.equal True
                        ]
        ]
