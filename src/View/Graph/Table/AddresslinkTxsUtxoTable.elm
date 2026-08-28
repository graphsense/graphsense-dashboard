module View.Graph.Table.AddresslinkTxsUtxoTable exposing (prepareCSV)

import Api.Data
import Model.Currency exposing (assetFromBase)
import Model.Locale
import Util.Csv
import Util.Data as Data
import View.Locale as Locale


prepareCSV : Model.Locale.Model -> String -> Api.Data.LinkUtxo -> List ( String, String )
prepareCSV locModel network row =
    ( "Tx_hash", Util.Csv.string row.txHash )
        :: Util.Csv.valuesWithBaseCurrencyFloat "Value" row.outputValue locModel (assetFromBase network)
        ++ [ ( "Currency", Util.Csv.string <| String.toUpper network )
           , ( "Height", Util.Csv.int row.height )
           , ( "Timestamp_utc", Data.timestampToPosix row.timestamp |> Locale.timestampNormal locModel )
           ]
