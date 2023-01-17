module View.Graph.Table.TxsAccountTable exposing (..)

import Api.Data
import Config.View as View
import Css.View
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


init : Table Api.Data.TxAccount
init =
    Init.Graph.Table.initUnsorted filter


filter : String -> Api.Data.TxAccount -> Bool
filter f a =
    String.contains f a.txHash
        || String.contains f (String.fromInt a.height)
        || String.contains f a.fromAddress
        || String.contains f a.toAddress
        || String.contains (String.toLower f) (String.toLower a.currency)


titleTx : String
titleTx =
    "Transaction"


titleHeight : String
titleHeight =
    "Height"


titleTimestamp : String
titleTimestamp =
    "Timestamp"


titleSendingAddress : String
titleSendingAddress =
    "Sending address"


titleReceivingAddress : String
titleReceivingAddress =
    "Receiving address"


config : View.Config -> Table.Config Api.Data.TxAccount Msg
config vc =
    Table.customConfig
        { toId = .txHash
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                titleTx
                .txHash
                (\data ->
                    Util.View.truncate vc.theme.table.urlMaxLength data.txHash
                        |> text
                        |> List.singleton
                        |> a
                            [ Css.View.link vc |> css
                            , Route.txRoute
                                { currency = data.currency
                                , txHash = data.txHash
                                , table = Nothing
                                }
                                |> Route.graphRoute
                                |> toUrl
                                |> href
                            ]
                        |> List.singleton
                )
            , T.valueColumnWithoutCode vc .currency "Value" .value
            , T.stringColumn vc "Currency" (.currency >> String.toUpper)
            , T.intColumn vc titleHeight .height
            , T.timestampColumn vc titleTimestamp .timestamp
            , T.stringColumn vc titleSendingAddress (.fromAddress >> Util.View.truncate vc.theme.table.urlMaxLength)
            , T.stringColumn vc titleReceivingAddress (.toAddress >> Util.View.truncate vc.theme.table.urlMaxLength)
            ]
        , customizations = customizations vc
        }


n s =
    ( s, [] )


prepareCSV : Api.Data.TxAccount -> List ( ( String, List String ), String )
prepareCSV row =
    [ ( n "tx_hash", Util.Csv.string row.txHash )
    ]
        ++ Util.Csv.values "value" row.value
        ++ [ ( n "height", Util.Csv.int row.height )
           , ( n "timestamp", Util.Csv.int row.timestamp )
           , ( n "sending_address", Util.Csv.string row.fromAddress )
           , ( n "receiving_address", Util.Csv.string row.toAddress )
           ]
