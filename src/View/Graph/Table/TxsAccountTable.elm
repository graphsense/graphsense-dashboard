module View.Graph.Table.TxsAccountTable exposing (prepareCSV)

import Api.Data
import Model.Locale
import Time
import Util.Csv
import Util.Data as Data
import View.Locale as Locale


prepareCSV : Model.Locale.Model -> String -> Api.Data.TxAccount -> List ( String, String )
prepareCSV locModel network row =
    [ ( "Tx_hash", Util.Csv.string row.txHash )
    , ( "Token_tx_id", row.tokenTxId |> Maybe.map Util.Csv.int |> Maybe.withDefault (Util.Csv.string "") )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat "Value" row.value locModel { network = network, asset = row.currency }
        ++ [ ( "Currency", Util.Csv.string <| String.toUpper row.currency )
           , ( "Height", Util.Csv.int row.height )
           , ( "Timestamp_utc", Locale.timestampNormal { locModel | zone = Time.utc } <| Data.timestampToPosix row.timestamp )
           , ( "Sending_address", Util.Csv.string row.fromAddress )
           , ( "Receiving_address", Util.Csv.string row.toAddress )
           ]
