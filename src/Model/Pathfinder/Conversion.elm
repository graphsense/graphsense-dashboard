module Model.Pathfinder.Conversion exposing (Conversion, toIdString)

import Api.Data
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)


type alias Conversion =
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


toIdString : Conversion -> String
toIdString conversion =
    conversion.raw.fromAssetTransfer ++ "_" ++ conversion.raw.toAssetTransfer
