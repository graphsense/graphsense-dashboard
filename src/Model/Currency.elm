module Model.Currency exposing (AssetIdentifier, Currency(..), allZero, asset, assetFromBase, tokensToValue)

import Api.Data
import Tuple exposing (second)


type Currency
    = Coin
    | Fiat String


type alias AssetIdentifier =
    { network : String, asset : String }


nativeAsset : String -> String
nativeAsset network =
    -- the GAS coin of a network: L2s differ from their network code (arbitrum
    -- pays gas in ETH; "ARB" quotes the governance token). Mirrors the REST
    -- stats field `native_coin` — keep in sync until this is seeded from /stats.
    case network of
        "arb" ->
            "eth"

        _ ->
            network


assetFromBase : String -> AssetIdentifier
assetFromBase network =
    { network = network, asset = nativeAsset network }


asset : String -> String -> AssetIdentifier
asset network assetName =
    -- currency == network code is the wire's NATIVE marker (token tickers may
    -- never equal the code), so such rows resolve to the network's gas coin
    if assetName == network then
        assetFromBase network

    else
        { network = network, asset = assetName }


tokensToValue : String -> List ( String, Api.Data.Values ) -> List ( AssetIdentifier, Api.Data.Values )
tokensToValue curr tokens =
    tokens |> List.map (\( x, v ) -> ( asset curr x, v ))


allZero : List ( AssetIdentifier, Api.Data.Values ) -> Bool
allZero =
    List.all (second >> .value >> (==) 0)
