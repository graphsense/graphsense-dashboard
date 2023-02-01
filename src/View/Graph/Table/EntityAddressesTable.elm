module View.Graph.Table.EntityAddressesTable exposing (..)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Address as A
import Model.Graph.Id exposing (EntityId)
import Model.Graph.Table as T exposing (Table)
import Msg.Graph exposing (Msg(..))
import Table
import Util.Csv
import Util.View exposing (none)
import View.Graph.Table as T exposing (customizations, valueColumn)


init : Table Api.Data.Address
init =
    Init.Graph.Table.initUnsorted filter


filter : String -> Api.Data.Address -> Bool
filter f a =
    String.contains f a.address


titleAddress : String
titleAddress =
    "Address"


titleFirstUsage : String
titleFirstUsage =
    "First usage"


titleLastUsage : String
titleLastUsage =
    "Last usage"


titleFinalBalance : String
titleFinalBalance =
    "Final balance"


titleTotalReceived : String
titleTotalReceived =
    "Total received"


config : View.Config -> String -> Maybe EntityId -> (EntityId -> A.Address -> Bool) -> Table.Config Api.Data.Address Msg
config vc coinCode entityId entityHasAddress =
    Table.customConfig
        { toId = .address
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                titleAddress
                .address
                (\data ->
                    [ entityId
                        |> Maybe.map
                            (\id ->
                                T.tickIf vc
                                    (entityHasAddress id)
                                    { currency = data.currency, address = data.address }
                            )
                        |> Maybe.withDefault none
                    , span
                        (entityId
                            |> Maybe.map
                                (\id ->
                                    [ UserClickedAddressInEntityAddressesTable id data
                                        |> onClick
                                    , css [ cursor pointer ]
                                    ]
                                )
                            |> Maybe.withDefault []
                        )
                        [ text data.address
                        ]
                    ]
                )
            , T.timestampColumn vc titleFirstUsage (.firstTx >> .timestamp)
            , T.timestampColumn vc titleLastUsage (.lastTx >> .timestamp)
            , T.valueColumn vc (\_ -> coinCode) titleFirstUsage .balance
            , T.valueColumn vc (\_ -> coinCode) titleTotalReceived .totalReceived
            ]
        , customizations = customizations vc
        }


n s =
    ( s, [] )


prepareCSV : Api.Data.Address -> List ( ( String, List String ), String )
prepareCSV row =
    [ ( n "address", Util.Csv.string row.address )
    , ( n "first_usage", Util.Csv.int row.firstTx.timestamp )
    , ( n "last_usage", Util.Csv.int row.lastTx.timestamp )
    ]
        ++ Util.Csv.values "final_balance" row.totalReceived
        ++ Util.Csv.values "total_received" row.balance
