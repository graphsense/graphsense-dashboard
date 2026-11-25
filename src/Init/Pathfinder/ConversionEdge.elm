module Init.Pathfinder.ConversionEdge exposing (init, initFromRow)

import Api.Data
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
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
    , legTxs = Just { input = inTx, output = outTx }
    , inputAddress = Nothing
    , outputAddress = Nothing
    , raw = apiConversion
    , selected = False
    , hovered = False
    , nodeOffset = Nothing
    }


{-| An edge drawn from its conversion row alone
(`Model.Pathfinder.ConversionEdge.drawsFromRow`): keyed by the row, between the
row's payer and payee, with no leg tx. Its labels come from the row, so the
asset fields stay empty.
-}
initFromRow : Api.Data.ExternalConversion -> ConversionEdge
initFromRow apiConversion =
    { id = ConversionEdge.rowEdgeId apiConversion
    , inputAddressId = ConversionEdge.payerId apiConversion
    , outputAddressId = ConversionEdge.payeeId apiConversion
    , fromAsset = ""
    , toAsset = ""
    , legTxs = Nothing
    , inputAddress = Nothing
    , outputAddress = Nothing
    , raw = apiConversion
    , selected = False
    , hovered = False
    , nodeOffset = Nothing
    }
