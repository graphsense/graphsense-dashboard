module View.Graph.Table.AddressNeighborsTable exposing (..)

import Api.Data
import Config.View as View
import Css exposing (cursor, pointer)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Table
import Model.Graph.Id exposing (AddressId)
import Model.Graph.Table exposing (Table)
import Msg.Graph exposing (Msg(..))
import Table
import View.Graph.Table as T exposing (customizations, valueColumn)
import View.Locale as Locale


init : Table Api.Data.NeighborAddress
init =
    Init.Graph.Table.init "Address"


config : View.Config -> Bool -> String -> Maybe AddressId -> Table.Config Api.Data.NeighborAddress Msg
config vc isOutgoing coinCode id =
    Table.customConfig
        { toId = .address >> .address
        , toMsg = TableNewState
        , columns =
            [ T.htmlColumn vc
                "Address"
                (.address >> .address)
                (\data ->
                    text data.address.address
                        |> List.singleton
                        |> div
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
                        |> List.singleton
                )
            , T.stringColumn vc "Labels" (.labels >> Maybe.withDefault [] >> reduceLabels)
            , T.valueColumn vc coinCode "Address balance" (.address >> .balance)
            , T.valueColumn vc coinCode "Address received" (.address >> .totalReceived)
            , T.intColumn vc "No. transactions" .noTxs
            , T.valueColumn vc coinCode "Estimated value" .value
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



{-
     name: t('Labels'),
     data: 'labels',
     render: (value, type) => {
       const maxCount = 30
       let charCount = 0
       let output = ''
       const labels = (value || []).sort()
       for (let i = 0; i < labels.length; i++) {
         const label = labels[i]
         charCount += label.length
         if (i > 0 && charCount > maxCount) {
           output += ` + ${labels.length - i}`
           break
         } else {
           if (i > 0) output += ', '
           output += label.length < maxCount ? label : (label.substr(0, maxCount) + '...')
         }
       }
       return `<span title="${value}">${output}</span>`
     }
   },
   {
     name: t('address/entity balance', t(type)),
     data: row => this.getValueByCurrencyCode(row.balance),
     className: 'text-right',
     render: (value, type) =>
       this.formatCurrencyInTable(type, value, keyspace, true)
   },
   {
     name: t('address/entity received', t(type)),
     data: row => this.getValueByCurrencyCode(row.received),
     className: 'text-right',
     render: (value, type) =>
       this.formatCurrencyInTable(type, value, keyspace, true)
   },
   {
     name: t('No. transactions'),
     data: 'no_txs',
     className: 'text-right'
   },
   {
     name: t('Estimated value'),
     data: row => this.getValueByCurrencyCode(row.value),
     className: 'text-right',
     render: (value, type) =>
       this.formatCurrencyInTable(type, value, keyspace, true)
   },
   {
     // just to enable full search of lables
     name: 'Labels',
     data: 'labels',
     render: (value) => (value || []).join(' '),
     visible: false
-}
