module View.Pathfinder.Table.IoTable exposing (..)

import Api.Data
import Config.View as View
import Css.Table exposing (Styles)
import Init.Pathfinder.Id as Id
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..), TxDetailsMsg(..))
import Table
import View.Graph.Table exposing (customizations)
import View.Pathfinder.Table.Columns as PT


config : Styles -> View.Config -> String -> (Id -> Bool) -> Maybe (Id -> HavingTags) -> Table.Config Api.Data.TxValue Msg
config styles vc network isCheckedFn lblFn =
    let
        toId =
            .address
                >> List.head
                >> Maybe.map (Id.init network)
    in
    Table.customConfig
        { toId = .address >> String.join ""
        , toMsg = TableMsg >> TxDetailsMsg
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
                (lblFn |> Maybe.map (\fn -> \data -> toId data |> Maybe.map fn |> Maybe.withDefault NoTags))
            , PT.debitCreditColumn vc
                (\_ -> assetFromBase network)
                "Value"
                .value
            ]
        , customizations = customizations styles vc
        }
