module Model.Pathfinder.ConversionEdge exposing (ConversionEdge, getInputTransferId, getInputTransferIdRaw, getOutputTransferId, getOutputTransferIdRaw, inputValues, nativeAsset, outputValues, toIdString)

import Api.Data
import Init.Pathfinder.Id as Id
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx
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

    -- drag offset of the swap icon from its layout position (graph units); Nothing = unmoved
    , nodeOffset : Maybe { x : Float, y : Float }
    }


{-| The asset a conversion leg names for the network's own coin.
-}
nativeAsset : String
nativeAsset =
    "native"


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


{-| The input leg's amount from the loaded leg tx, quoted with the conversion's
own fiat when sent (the leg tx may be unpriced). A leg tx not touching
`fromAddress` reads as zero with no fiat.
-}
inputValues : ConversionEdge -> Api.Data.Values
inputValues c =
    Tx.getInputValueForAddressFromRawTx c.raw.fromAddress c.rawInputTransaction
        |> legValues c.raw.fromAmountFiatValues


{-| The same for the output leg.
-}
outputValues : ConversionEdge -> Api.Data.Values
outputValues c =
    Tx.getOutputValueForAddressFromRawTx c.raw.toAddress c.rawOutputTransaction
        |> legValues c.raw.toAmountFiatValues


legValues : Maybe (List Api.Data.Rate) -> Maybe Api.Data.Values -> Api.Data.Values
legValues rates =
    Maybe.map (withFiat rates)
        >> Maybe.withDefault { value = 0, fiatValues = [] }


withFiat : Maybe (List Api.Data.Rate) -> Api.Data.Values -> Api.Data.Values
withFiat rates values =
    rates
        |> Maybe.map (\rs -> { values | fiatValues = rs })
        |> Maybe.withDefault values
