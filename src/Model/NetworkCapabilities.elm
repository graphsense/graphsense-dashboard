module Model.NetworkCapabilities exposing
    ( Capability(..)
    , NetworkCapabilities
    , fromBuildConfig
    , isLimitedNetwork
    , supports
    , withCapabilities
    )

{-| Which core-GraphSense features each network's serving backend answers.

Wire contract (GET /capabilities, sent by external-backend deployments):
per-network DISABLED feature flags. A network absent from the response is
fully enabled; unknown vocabulary words must be tolerated (they are stored
but map to no `Capability`). Old servers 404 the endpoint — the response
then never arrives and only the build-time seed applies. This module is the
only place that parses the contract — everything else asks `supports` /
`isLimitedNetwork`.

The build-time `Config.limitedNetworks` list seeds networks as
fully-disabled lite entries, covering requests fired before /capabilities
has arrived and backends without the endpoint.

-}

import Api.Data
import Dict exposing (Dict)
import Set exposing (Set)


type NetworkCapabilities
    = NetworkCapabilities (Dict String (Set String))


type Capability
    = Relations
    | Clusters
    | Tags
    | Conversions


capabilityKey : Capability -> String
capabilityKey capability =
    case capability of
        Relations ->
            "relations"

        Clusters ->
            "clusters"

        Tags ->
            "tags"

        Conversions ->
            "conversions"


{-| Every capability word the seed disables — includes flags without a
`Capability` constructor yet (exact\_stats), which only consumers gaining
one later will read.
-}
allCapabilityKeys : Set String
allCapabilityKeys =
    Set.fromList [ "relations", "clusters", "tags", "conversions", "exact_stats" ]


fromBuildConfig : List String -> NetworkCapabilities
fromBuildConfig networks =
    networks
        |> List.map (\network -> ( String.toLower network, allCapabilityKeys ))
        |> Dict.fromList
        |> NetworkCapabilities


{-| Take a /capabilities response as the whole truth: the server knows its
deployment, so its declaration replaces the build-config seed entirely — a
network absent from the response (or listing no disabled features) is fully
enabled.
-}
withCapabilities : Api.Data.Capabilities -> NetworkCapabilities -> NetworkCapabilities
withCapabilities capabilities _ =
    capabilities.networks
        |> List.map
            (\entry ->
                ( String.toLower entry.network
                , entry.disabled |> List.map String.toLower |> Set.fromList
                )
            )
        |> Dict.fromList
        |> NetworkCapabilities


{-| Lite = at least one feature is disabled (declared by the backend or
seeded by the build config). Networks without disabled features are core.
-}
isLimitedNetwork : NetworkCapabilities -> String -> Bool
isLimitedNetwork (NetworkCapabilities networks) network =
    Dict.get (String.toLower network) networks
        |> Maybe.map (Set.isEmpty >> not)
        |> Maybe.withDefault False


supports : Capability -> NetworkCapabilities -> String -> Bool
supports capability (NetworkCapabilities networks) network =
    case Dict.get (String.toLower network) networks of
        Nothing ->
            True

        Just disabled ->
            Set.member (capabilityKey capability) disabled |> not
