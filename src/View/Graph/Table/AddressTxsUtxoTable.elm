module View.Graph.Table.AddressTxsUtxoTable exposing (..)

import Api.Data
import Config.View as View
import Css.View
import Csv.Encode
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Graph.Id exposing (AddressId)
import Model.Graph.Table exposing (Table)
import Msg.Graph exposing (Msg(..))
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Util.Csv
import Util.View
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale
import View.Util exposing (longIdentifier)


init : Table Api.Data.AddressTxUtxo
init =
    Init.Graph.Table.initSorted True filter titleTimestamp


filter : String -> Api.Data.AddressTxUtxo -> Bool
filter f a =
    String.contains f (String.fromInt a.height)
        || String.contains f a.txHash


titleTx : String
titleTx =
    "Transaction"


titleValue : String
titleValue =
    "Value"


titleHeight : String
titleHeight =
    "Height"


titleTimestamp : String
titleTimestamp =
    "Timestamp"


config : View.Config -> String -> Table.Config Api.Data.AddressTxUtxo Msg
config vc coinCode =
    Table.customConfig
        { toId = .txHash
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                titleTx
                .txHash
                (\data ->
                    longIdentifier vc data.txHash
                        |> List.singleton
                        |> a
                            [ Css.View.link vc |> css
                            , Route.txRoute
                                { currency = coinCode
                                , txHash = data.txHash
                                , table = Nothing
                                , tokenTxId = Nothing
                                }
                                |> Route.graphRoute
                                |> toUrl
                                |> href
                            ]
                        |> List.singleton
                )
            , T.valueColumn vc (\_ -> coinCode) titleValue .value
            , T.intColumnWithoutValueDetailFormatting vc titleHeight .height
            , T.timestampColumn vc titleTimestamp .timestamp
            ]
        , customizations = customizations vc
        }


prepareCSV : Api.Data.AddressTxUtxo -> List ( ( String, List String ), String )
prepareCSV row =
    [ ( ( "tx_hash", [] ), Util.Csv.string row.txHash )
    ]
        ++ Util.Csv.values "value" row.value
        ++ [ ( ( "height", [] ), Util.Csv.int row.height )
           , ( ( "timestamp", [] ), Util.Csv.int row.timestamp )
           ]
