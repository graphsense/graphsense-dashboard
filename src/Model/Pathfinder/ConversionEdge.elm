module Model.Pathfinder.ConversionEdge exposing (ConversionEdge, getUniqueConversions, toIdString)

import Api.Data
import List.Extra
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)


type alias ConversionEdge =
    { outputId : Id
    , inputId : Id
    , fromAsset : String
    , toAsset : String
    , inputAddress : Maybe Address
    , outputAddress : Maybe Address
    , raw : Api.Data.ExternalConversion
    , selected : Bool
    , hovered : Bool
    }


toIdString : ConversionEdge -> String
toIdString conversion =
    conversion.raw.fromAssetTransfer ++ "_" ++ conversion.raw.toAssetTransfer


getUniqueConversions : List ConversionEdge -> List ConversionEdge
getUniqueConversions list =
    list
        |> List.Extra.uniqueBy toIdString
