module View.Pathfinder.Table.IoTable exposing (..)

import Api.Data
import Config.View as View
import Css.Pathfinder as Css
import Init.Pathfinder.Id as Id
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Table
import View.Pathfinder.Table exposing (customizations)
import View.Pathfinder.Table.Columns as PT


customizationsIO : View.Config -> Table.Customizations data msg
customizationsIO vc =
    let
        tc =
            customizations vc
    in
    { tc | tableAttrs = tc.tableAttrs ++ [ Css.ioTableStyle vc |> Css.toAttr ] }


config : View.Config -> String -> (Id -> Bool) -> Table.Config Api.Data.TxValue Msg
config vc network isCheckedFn =
    let
        toId =
            .address
                >> List.head
                >> Maybe.map (Id.init network)
    in
    Table.customConfig
        { toId = .address >> String.join ""
        , toMsg = \_ -> NoOp
        , columns =
            [ PT.checkboxColumn vc
                { isChecked =
                    toId
                        >> Maybe.map isCheckedFn
                        >> Maybe.withDefault False
                , onClick =
                    toId >> Maybe.map UserClickedAddressCheckboxInTable >> Maybe.withDefault NoOp
                }
            , PT.addressColumn vc
                { label = "Address"
                , accessor = .address >> String.join ","
                , onClick = Nothing
                }
            , PT.debitCreditColumn vc
                (\_ -> assetFromBase network)
                "Value"
                .value
            ]
        , customizations = customizationsIO vc
        }
