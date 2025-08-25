module View.Graph.Table.TxUtxoTable exposing (config, prepareCSV)

import Api.Data
import Components.Table as Table
import Config.View as View
import Css
import Css.Table exposing (styles)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Currency exposing (assetFromBase)
import Model.Graph.Table.TxUtxoTable exposing (..)
import Model.Locale
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.View exposing (copyableLongIdentifier)
import View.Graph.Table as T exposing (customizations)


columnTitleFromDirection : Bool -> String
columnTitleFromDirection isOutgoing =
    (if isOutgoing then
        "Outgoing"

     else
        "Incoming"
    )
        ++ " address"


config : View.Config -> Bool -> String -> Table.Config Api.Data.TxValue Msg
config vc isOutgoing coinCode =
    Table.customConfig
        { toId = \data -> String.join "," data.address ++ String.fromInt data.value.value
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn styles
                vc
                (columnTitleFromDirection isOutgoing)
                joinAddresses
                (\data ->
                    [ joinAddresses data
                        |> copyableLongIdentifier vc
                            (data.address
                                |> List.head
                                |> Maybe.map
                                    (\one ->
                                        [ UserClickedAddressInTable
                                            { address = one
                                            , currency = coinCode
                                            }
                                            |> onClick
                                        , css [ Css.cursor Css.pointer ]
                                        ]
                                    )
                                |> Maybe.withDefault []
                            )
                    ]
                )
            , T.valueColumn styles vc (\_ -> assetFromBase coinCode) titleValue .value
            ]
        , customizations = customizations styles vc
        }


prepareCSV : Model.Locale.Model -> String -> Bool -> Api.Data.TxValue -> List ( ( String, List String ), String )
prepareCSV locModel currency isOutgoing row =
    ( ( "addresses", [] ), Util.Csv.string <| joinAddresses row )
        :: Util.Csv.valuesWithBaseCurrencyFloat "value" row.value locModel currency
