module View.Graph.Table.AddressNeighborsTable exposing (..)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Address as A
import Model.Graph.Id exposing (AddressId)
import Model.Graph.Table exposing (Table)
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.View exposing (none)
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale


columnTitleFromDirection : Bool -> String
columnTitleFromDirection isOutgoing =
    (if isOutgoing then
        "Outgoing"

     else
        "Incoming"
    )
        ++ " address"


init : Bool -> Table Api.Data.NeighborAddress
init =
    columnTitleFromDirection
        >> Init.Graph.Table.init filter


filter : String -> Api.Data.NeighborAddress -> Bool
filter f a =
    String.contains f a.address.address
        || (Maybe.map (List.any (String.contains f)) a.labels |> Maybe.withDefault True)


titleLabels : String
titleLabels =
    "Labels"


titleAddressBalance : String
titleAddressBalance =
    "Address balance"


titleAddressReceived : String
titleAddressReceived =
    "Address received"


titleNoTxs : String
titleNoTxs =
    "No. transactions"


titleEstimatedValue : String
titleEstimatedValue =
    "Estimated value"


config : View.Config -> Bool -> String -> Maybe AddressId -> (AddressId -> Bool -> A.Address -> Bool) -> Table.Config Api.Data.NeighborAddress Msg
config vc isOutgoing coinCode id neighborLayerHasAddress =
    Table.customConfig
        { toId = .address >> .address
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                (columnTitleFromDirection isOutgoing)
                (.address >> .address)
                (\data ->
                    [ id
                        |> Maybe.map
                            (\aid ->
                                T.tickIf vc
                                    (neighborLayerHasAddress aid isOutgoing)
                                    { currency = data.address.currency
                                    , address = data.address.address
                                    }
                            )
                        |> Maybe.withDefault none
                    , span
                        (id
                            |> Maybe.map
                                (\addressId ->
                                    [ UserClickedAddressInNeighborsTable addressId isOutgoing data
                                        |> onClick
                                    , css [ cursor pointer ]
                                    ]
                                )
                            |> Maybe.withDefault []
                        )
                        [ text data.address.address
                        ]
                    ]
                )
            , T.stringColumn vc titleLabels (.labels >> Maybe.withDefault [] >> reduceLabels)
            , T.valueColumn vc (\_ -> coinCode) titleAddressBalance (.address >> .balance)
            , T.valueColumn vc (\_ -> coinCode) titleAddressReceived (.address >> .totalReceived)
            , T.intColumn vc titleNoTxs .noTxs
            , T.valueColumn vc (\_ -> coinCode) titleEstimatedValue .value
            ]
        , customizations = customizations vc
        }


reduceLabels : List String -> String
reduceLabels labels =
    let
        maxCount =
            30
    in
    labels
        |> List.foldl
            (\label acc ->
                if acc.i > 0 && acc.charCount > maxCount then
                    { acc
                        | output =
                            acc.output ++ " + " ++ (List.length labels - acc.i |> String.fromInt)
                    }

                else
                    { output =
                        acc.output
                            ++ (if acc.i > 0 then
                                    ", "

                                else
                                    ""
                               )
                            ++ (if String.length label < maxCount then
                                    label

                                else
                                    String.left maxCount label ++ "…"
                               )
                    , i = acc.i + 1
                    , charCount = acc.charCount + String.length label
                    }
            )
            { charCount = 0
            , i = 0
            , output = ""
            }
        |> .output


n s =
    ( s, [] )


prepareCSV : Bool -> Api.Data.NeighborAddress -> List ( ( String, List String ), String )
prepareCSV isOutgoing row =
    [ ( n <| "address", Util.Csv.string row.address.address )
    , ( n "labels", row.labels |> Maybe.withDefault [] |> String.join ", " |> Util.Csv.string )
    , ( n "no_txs", Util.Csv.int row.noTxs )
    ]
        ++ Util.Csv.values "address_balance" row.address.totalReceived
        ++ Util.Csv.values "address_received" row.address.balance
        ++ Util.Csv.values "estimated_value" row.value
