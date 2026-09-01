module Model.NetworkCapabilities exposing
    ( Capability(..)
    , NetworkCapabilities
    , fromApi
    , isLiteNetwork
    , none
    , supports
    )

{-| Which optional features the backend serves per network.

`GET /capabilities` lists, per network, the features that are DISABLED. A
network absent from the response is fully enabled, and so is every network
when the endpoint does not exist (older backends answer 404). Words the app
does not know are kept, so a network that only disables something we have no
constructor for still counts as lite. This module is the only place that
reads the wire format; everything else asks `supports` or `isLiteNetwork`.

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
    | ExactStats


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

        ExactStats ->
            "exact_stats"


{-| Every network fully enabled — the state before the response arrives and
the state a backend without the endpoint leaves us in.
-}
none : NetworkCapabilities
none =
    NetworkCapabilities Dict.empty


fromApi : Api.Data.Capabilities -> NetworkCapabilities
fromApi capabilities =
    capabilities.networks
        |> List.map
            (\entry ->
                ( String.toLower entry.network
                , entry.disabled |> List.map String.toLower |> Set.fromList
                )
            )
        |> Dict.fromList
        |> NetworkCapabilities


{-| At least one feature is disabled on this network.
-}
isLiteNetwork : NetworkCapabilities -> String -> Bool
isLiteNetwork (NetworkCapabilities networks) network =
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
