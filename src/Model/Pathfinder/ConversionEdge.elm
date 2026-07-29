module Model.Pathfinder.ConversionEdge exposing (ConversionEdge, getInputTransferId, getInputTransferIdRaw, getOutputTransferId, getOutputTransferIdRaw, toIdString)

import Api.Data
import Init.Pathfinder.Id as Id
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Util exposing (removeLeading0x)


type alias ConversionEdge =
    { id : ( Id, Id )
    , outputAddressId : Id
    , inputAddressId : Id
    , fromAsset : String
    , toAsset : String
    , inputAddress : Maybe Address
    , outputAddress : Maybe Address
    , rawInputTransaction : Api.Data.Tx
    , rawOutputTransaction : Api.Data.Tx
    , raw : Api.Data.ExternalConversion
    , selected : Bool
    , hovered : Bool
    }


getOutputTransferIdRaw : Api.Data.ExternalConversion -> Id
getOutputTransferIdRaw conversion =
    Id.init conversion.toNetwork (conversion.toAssetTransfer |> removeLeading0x)


getInputTransferIdRaw : Api.Data.ExternalConversion -> Id
getInputTransferIdRaw conversion =
    Id.init conversion.fromNetwork (conversion.fromAssetTransfer |> removeLeading0x)


getOutputTransferId : ConversionEdge -> Id
getOutputTransferId conversion =
    conversion.raw |> getOutputTransferIdRaw


getInputTransferId : ConversionEdge -> Id
getInputTransferId conversion =
    conversion.raw |> getInputTransferIdRaw


toIdString : ConversionEdge -> String
toIdString conversion =
    conversion.raw.fromAssetTransfer ++ "_" ++ conversion.raw.toAssetTransfer
