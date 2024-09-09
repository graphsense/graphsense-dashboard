module View.Pathfinder.Table.IoTable exposing (..)

import Api.Data
import Config.View as View
import Css
import Css.Table exposing (Styles)
import Html.Styled.Attributes as HA exposing (id, src)
import Init.Pathfinder.Id as Id
import Model.Currency exposing (assetFromBase)
import Model.Pathfinder exposing (HavingTags(..))
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..), TxDetailsMsg(..))
import RecordSetter exposing (..)
import Set
import Table
import Tuple3
import View.Graph.Table exposing (customizations, simpleThead)
import View.Pathfinder.PagedTable exposing (alignColumnsRight)
import View.Pathfinder.Table.Columns as PT


config : Styles -> View.Config -> String -> (Id -> Bool) -> Maybe (Id -> HavingTags) -> Table.Config Api.Data.TxValue Msg
config styles vc network isCheckedFn lblFn =
    let
        toId =
            .address
                >> List.head
                >> Maybe.map (Id.init network)

        rightAlignedColumns =
            [ "Value" ]
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
                , tagsPlaceholder = True
                }
                (lblFn |> Maybe.map (\fn -> \data -> toId data |> Maybe.map fn |> Maybe.withDefault NoTags))
            , PT.debitCreditColumn
                (.value >> .value >> (>=) 0)
                vc
                (\_ -> assetFromBase network)
                "Value"
                .value
            ]
        , customizations = customizations styles vc |> alignColumnsRight vc (Set.fromList rightAlignedColumns)
        }
