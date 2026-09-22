module Util.TokenConfigs exposing (merge, register)

{-| A network's token configs have two writers: the `/supported_tokens` list the
dashboard fetches per network, and the curated assets a served dex swap names
(`RegisterConversionAsset`). They write the same `supportedTokens` entry and
arrive in either order, so both writes go through here.

Contract addresses are the identity, compared case-insensitively: the API
serves checksummed addresses, a swap leg lowercased ones, and the same token
must not end up in the list twice.

-}

import Api.Data
import Set


{-| The token list that just arrived, keeping the registered assets it does not
carry itself. The list wins on collision — it is the network's own answer, the
registration only a stand-in for a token the list was missing.
-}
merge : Api.Data.TokenConfigs -> Api.Data.TokenConfigs -> Api.Data.TokenConfigs
merge incoming existing =
    let
        arrived =
            incoming.tokenConfigs
                |> List.filterMap (.contractAddress >> Maybe.map String.toLower)
                |> Set.fromList

        kept =
            existing.tokenConfigs
                |> List.filter
                    (\config ->
                        case config.contractAddress of
                            Just address ->
                                not (Set.member (String.toLower address) arrived)

                            Nothing ->
                                False
                    )
    in
    { tokenConfigs = incoming.tokenConfigs ++ kept }


{-| Adds a curated asset unless its contract address is already known.
-}
register : Api.Data.TokenConfig -> Api.Data.TokenConfigs -> Api.Data.TokenConfigs
register config existing =
    let
        sameAddress other =
            Maybe.map String.toLower other.contractAddress
                == Maybe.map String.toLower config.contractAddress
    in
    if List.any sameAddress existing.tokenConfigs then
        existing

    else
        { tokenConfigs = config :: existing.tokenConfigs }
