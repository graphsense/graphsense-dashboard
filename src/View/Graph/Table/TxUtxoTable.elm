module View.Graph.Table.TxUtxoTable exposing (..)

import Api.Data
import Config.View as View
import Css
import Css.View
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Graph.Table as T exposing (Table)
import Msg.Graph exposing (Msg(..))
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Util.Csv
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


init : Bool -> Table Api.Data.TxValue
init =
    columnTitleFromDirection
        >> Init.Graph.Table.init filter


filter : String -> Api.Data.TxValue -> Bool
filter f a =
    List.any (String.contains f) a.address


titleValue : String
titleValue =
    "Value"


joinAddresses : Api.Data.TxValue -> String
joinAddresses =
    .address >> String.join ","


config : View.Config -> Bool -> String -> Table.Config Api.Data.TxValue Msg
config vc isOutgoing coinCode =
    Table.customConfig
        { toId = \data -> String.join "," data.address ++ String.fromInt data.value.value
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                (columnTitleFromDirection isOutgoing)
                joinAddresses
                (\data ->
                    [ case data.address of
                        one :: [] ->
                            span
                                [ UserClickedAddressInTable
                                    { address = one
                                    , currency = coinCode
                                    }
                                    |> onClick
                                , css [ Css.cursor Css.pointer ]
                                ]
                                [ text one
                                ]

                        _ ->
                            joinAddresses data
                                |> text
                    ]
                )
            , T.valueColumn vc coinCode titleValue .value
            ]
        , customizations = customizations vc
        }


prepareCSV : Bool -> Api.Data.TxValue -> List ( ( String, List String ), String )
prepareCSV isOutgoing row =
    [ ( ( columnTitleFromDirection isOutgoing, [] ), Util.Csv.string <| joinAddresses row )
    ]
        ++ Util.Csv.values (Util.Csv.a0 titleValue) row.value
