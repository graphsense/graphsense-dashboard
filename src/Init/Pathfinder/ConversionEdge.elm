module Init.Pathfinder.ConversionEdge exposing (init)

import Api.Data
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx
import Tuple exposing (first, second)


init : Api.Data.ExternalConversion -> ( Id, Id ) -> ( Id, Id ) -> Api.Data.Tx -> Api.Data.Tx -> ConversionEdge
init apiConversion id addressIds inTx outTx =
    { id = id
    , inputAddressId = first addressIds
    , outputAddressId = second addressIds
    , fromAsset = inTx |> Tx.getAssetFromRawTx |> String.toUpper
    , toAsset = outTx |> Tx.getAssetFromRawTx |> String.toUpper
    , rawInputTransaction = inTx
    , rawOutputTransaction = outTx
    , inputAddress = Nothing
    , outputAddress = Nothing
    , raw = apiConversion
    , selected = False
    , hovered = False
    }
