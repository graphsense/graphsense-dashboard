module Model.Currency exposing (AssetIdentifier, Currency(..), allZero, asset, assetFromBase, tokensToValue)

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


tokensToValue : String -> List ( String, Api.Data.Values ) -> List ( AssetIdentifier, Api.Data.Values )
tokensToValue curr tokens =
    tokens |> List.map (\( x, v ) -> ( asset curr x, v ))


allZero : List ( AssetIdentifier, Api.Data.Values ) -> Bool
allZero =
    List.all (second >> .value >> (==) 0)
