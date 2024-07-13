module View.Pathfinder.Table.TransactionTable exposing (config, prepareCSV)

import Api.Data
import Config.View as View
import Css.Table exposing (Styles)
import Html.Styled
import Init.Pathfinder.Id as Id
import Model.Currency exposing (asset)
import Model.Locale
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Table
import Theme.Html.SidebarComponents as SidebarComponents
import Util.View exposing (truncateLongIdentifier)
import View.Graph.Table
import View.Locale as Locale
import View.Pathfinder.Table exposing (customizations)
import View.Pathfinder.Table.Columns as PT


type alias GenericTx =
    { currency : String
    , txHash : String
    , timestamp : Int
    , value : Api.Data.Values
    , asset : String
    }


toGerneric : Api.Data.AddressTx -> GenericTx
toGerneric x =
    case x of
        Api.Data.AddressTxAddressTxUtxo y ->
            GenericTx y.currency y.txHash y.timestamp y.value y.currency

        Api.Data.AddressTxTxAccount y ->
            GenericTx y.currency y.txHash y.timestamp y.value y.currency


getId : GenericTx -> Id
getId { currency, txHash } =
    Id.init currency txHash


config : Styles -> View.Config -> String -> (Id -> Bool) -> Table.Config Api.Data.AddressTx Msg
config styles vc network isCheckedFn =
    Table.customConfig
        { toId = toGerneric >> getId >> Id.toString
        , toMsg = \_ -> NoOp
        , columns =
            [ PT.checkboxColumn vc
                { isChecked = toGerneric >> getId >> isCheckedFn
                , onClick = UserClickedTxCheckboxInTable
                }
            , View.Graph.Table.htmlColumn
                styles
                vc
                "Timestamp"
                (toGerneric >> .timestamp)
                (\data ->
                    let
                        d =
                            toGerneric data |> .timestamp
                    in
                    SidebarComponents.txListCellTimestamp
                        { checkboxes = {}
                        , txListCellTimestamp =
                            { checkbox = False
                            , date =
                                Locale.timestampDateUniform vc.locale d
                            , time =
                                Locale.timestampTimeUniform vc.locale d
                            }
                        }
                        |> List.singleton
                )
            , View.Graph.Table.htmlColumnWithSorter
                Table.unsortable
                styles
                vc
                "Hash"
                (toGerneric >> .txHash)
                (\data ->
                    let
                        d =
                            toGerneric data
                    in
                    SidebarComponents.txListCellValue
                        { txListCellValue =
                            { txValue = truncateLongIdentifier d.txHash
                            }
                        }
                        |> List.singleton
                )
            , PT.valueColumnWithOptions
                { sortable = False
                , hideCode = False
                , hideFlowIndicator = False
                }
                vc
                (toGerneric >> .asset >> asset network)
                "Debit/Credit"
                (toGerneric >> .value)
            ]
        , customizations = customizations vc
        }


prepareCSV : Model.Locale.Model -> String -> Api.Data.AddressTx -> List ( ( String, List String ), String )
prepareCSV _ _ _ =
    []
