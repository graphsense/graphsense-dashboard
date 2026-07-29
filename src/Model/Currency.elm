module Model.Currency exposing (AssetIdentifier, Currency(..), allZero, asset, assetFromBase)

import Api.Data
import Tuple exposing (second)


type Currency
    = Coin
    | Fiat String


type alias AssetIdentifier =
    { network : String, asset : String }


assetFromBase : String -> AssetIdentifier
assetFromBase network =
    { network = network, asset = network }


asset : String -> String -> AssetIdentifier
asset network assetName =
    { network = network, asset = assetName }


allZero : List ( AssetIdentifier, Api.Data.Values ) -> Bool
allZero =
    List.all (second >> .value >> (==) 0)
