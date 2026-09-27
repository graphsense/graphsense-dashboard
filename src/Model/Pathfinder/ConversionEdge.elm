module Model.Pathfinder.ConversionEdge exposing (ConversionEdge, getInputTransferId, getInputTransferIdRaw, getOutputTransferId, getOutputTransferIdRaw, inputValues, outputValues, toIdString)

import Api.Data
import Init.Pathfinder.Id as Id
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx
import Util exposing (removeLeading0x)
import Util.Data as Data


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

    -- where the user dragged the swap icon, relative to where the layout puts
    -- it (graph units, same space as the rendered curve); Nothing = unmoved
    , nodeOffset : Maybe { x : Float, y : Float }
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


{-| The amounts of the swap's input leg: the value the loaded leg transaction
moves, quoted with the conversion's own per-leg fiat rates when the backend
sent them (it prices the leg at the swap's asset and height, which the leg
transaction itself may not be priced at at all).

When the conversion's address is not on the leg transaction the amount falls
back to zero, and that zero carries no fiat: a quote beside it would price an
amount the panel does not show.

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
legValues rates matched =
    case matched of
        Just values ->
            withFiat rates values

        Nothing ->
            unmatchedLegValues


{-| The zero amount a leg falls back to, stripped of the zero quotes
`Data.valuesZero` carries.
-}
unmatchedLegValues : Api.Data.Values
unmatchedLegValues =
    { value = Data.valuesZero.value, fiatValues = [] }


withFiat : Maybe (List Api.Data.Rate) -> Api.Data.Values -> Api.Data.Values
withFiat rates values =
    case rates of
        Just rs ->
            { values | fiatValues = rs }

        Nothing ->
            values
