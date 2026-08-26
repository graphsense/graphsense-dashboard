module Model.NetworkCapabilities exposing
    ( Capability(..)
    , NetworkCapabilities
    , fromBuildConfig
    , isLimitedNetwork
    , supports
    , withStats
    )

{-| Which core-GraphSense features each network's serving backend answers.

Wire contract (the `capabilities` extension field of a per-currency /stats
entry, sent by external-backend deployments): ABSENT means full core
GraphSense; PRESENT (even empty) means a lite network limited to exactly the
named features. Unknown vocabulary words must be ignored. This module is the
only place that parses the contract — everything else asks `supports` /
`isLimitedNetwork`.

The build-time `Config.limitedNetworks` list seeds networks as
capability-less lite entries, covering backends that do not declare
themselves and requests fired before /stats has arrived.

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


fromBuildConfig : List String -> NetworkCapabilities
fromBuildConfig networks =
    networks
        |> List.map (\network -> ( String.toLower network, Set.empty ))
        |> Dict.fromList
        |> NetworkCapabilities


{-| Fold the declarations of a /stats response in. A currency declaring a
capabilities list overwrites its build-config seed (the server knows its
deployment better); a currency without the field leaves the seed untouched —
absence is the baseline's silence, not a statement of full support.
-}
withStats : Api.Data.Stats -> NetworkCapabilities -> NetworkCapabilities
withStats stats (NetworkCapabilities networks) =
    stats.currencies
        |> List.foldl
            (\currency acc ->
                case currency.capabilities of
                    Just capabilities ->
                        Dict.insert (String.toLower currency.name)
                            (capabilities |> List.map String.toLower |> Set.fromList)
                            acc

                    Nothing ->
                        acc
            )
            networks
        |> NetworkCapabilities


{-| Lite = the backend declared a capability subset (or the build config
listed the network). Core networks support every feature.
-}
isLimitedNetwork : NetworkCapabilities -> String -> Bool
isLimitedNetwork (NetworkCapabilities networks) network =
    Dict.member (String.toLower network) networks


supports : Capability -> NetworkCapabilities -> String -> Bool
supports capability (NetworkCapabilities networks) network =
    case Dict.get (String.toLower network) networks of
        Nothing ->
            True

        Just capabilities ->
            Set.member (capabilityKey capability) capabilities
