module View.Graph.Table.TxsAccountTable exposing (..)

import Api.Data
import Config.View as View
import Css.Table exposing (styles)
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Init.Graph.Table
import Model.Currency exposing (asset)
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.TxsAccountTable exposing (..)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Util.Csv
import Util.View exposing (longIdentifier)
import View.Graph.Table as T exposing (customizations)


blockConfig : View.Config -> String -> Table.Config Api.Data.TxAccount Msg
blockConfig vc coinCode =
    let
        toMsg field data =
            UserClickedAddressInTable
                { currency = coinCode
                , address = field data
                }
    in
    Table.customConfig
        { toId = .txHash
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn styles
                vc
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
                                , tokenTxId = data.tokenTxId
                                }
                                |> Route.graphRoute
                                |> toUrl
                                |> href
                            ]
                        |> List.singleton
                )
            ]
                ++ [ (if vc.locale.currency /= Model.Currency.Coin then
                        T.valueColumn

                      else
                        T.valueColumnWithoutCode
                     )
                        styles
                        vc
                        (\x -> asset coinCode x.currency)
                        "Value"
                        .value
                   ]
                ++ [ T.stringColumn styles vc "Currency" (.currency >> String.toUpper)
                   , toMsg .fromAddress
                        |> T.addressColumn styles vc titleSendingAddress .fromAddress
                   , toMsg .toAddress
                        |> T.addressColumn styles vc titleReceivingAddress .toAddress
                   ]
        , customizations = customizations styles vc
        }


config : View.Config -> String -> Table.Config Api.Data.TxAccount Msg
config vc coinCode =
    let
        toMsg field data =
            UserClickedAddressInTable
                { currency = coinCode
                , address = field data
                }
    in
    Table.customConfig
        { toId = .txHash
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn styles
                vc
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
                                , tokenTxId = data.tokenTxId
                                }
                                |> Route.graphRoute
                                |> toUrl
                                |> href
                            ]
                        |> List.singleton
                )
            ]
                ++ [ (if vc.locale.currency /= Model.Currency.Coin then
                        T.valueColumn

                      else
                        T.valueColumnWithoutCode
                     )
                        styles
                        vc
                        (\x -> asset coinCode x.currency)
                        "Value"
                        .value
                   ]
                ++ [ T.stringColumn styles vc "Currency" (.currency >> String.toUpper)
                   , T.timestampColumn styles vc titleTimestamp .timestamp
                   , toMsg .fromAddress
                        |> T.addressColumn styles vc titleSendingAddress .fromAddress
                   , toMsg .toAddress
                        |> T.addressColumn styles vc titleReceivingAddress .toAddress
                   , T.intColumnWithoutValueDetailFormatting styles vc titleHeight .height
                   , T.maybeIntColumn styles vc "Token Tx Id" .tokenTxId
                   ]
        , customizations = customizations styles vc
        }


n s =
    ( s, [] )


prepareCSV : Model.Locale.Model -> String -> Api.Data.TxAccount -> List ( ( String, List String ), String )
prepareCSV locModel currency row =
    [ ( n "tx_hash", Util.Csv.string row.txHash )
    , ( n "token_tx_id", row.tokenTxId |> Maybe.map Util.Csv.int |> Maybe.withDefault (Util.Csv.string "") )
    ]
        ++ Util.Csv.valuesWithBaseCurrencyFloat "value" row.value locModel row.currency
        ++ [ ( n "currency", Util.Csv.string row.currency )
           , ( n "height", Util.Csv.int row.height )
           , ( n "timestamp", Util.Csv.timestamp locModel row.timestamp )
           , ( n "sending_address", Util.Csv.string row.fromAddress )
           , ( n "receiving_address", Util.Csv.string row.toAddress )
           ]
