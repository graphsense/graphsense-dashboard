module View.Pathfinder.RelationDetails exposing (ValuesFormatted, ValuesRow, makeValuesList, view)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth, sidePanelCss)
import Css.Table
import Css.View
import Dict
import Html.Styled exposing (Html, div)
import Model.Currency as Currency exposing (AssetIdentifier)
import Model.Locale as Locale
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.RelationDetails as RelationDetails
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder as Pathfinder exposing (Msg(..))
import Msg.Pathfinder.RelationDetails as RelationDetails
import RecordSetter as Rs
import RemoteData exposing (RemoteData(..))
import Svg.Styled.Attributes exposing (css)
import Theme.Html.Icons as Icons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Tuple exposing (first, pair, second)
import Util exposing (allAndNotEmpty)
import Util.Css exposing (spread)
import Util.View exposing (loadingSpinner, none, truncateLongIdentifier)
import View.Locale as Locale
import View.Pathfinder.Details exposing (closeAttrs, dataTab)
import View.Pathfinder.InfiniteTable as InfiniteTable
import View.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import View.Pathfinder.TransactionFilter as TransactionFilter


isLeftToRight : RelationDetails.Model -> Bool
isLeftToRight viewState =
    Maybe.map2
        (\a b ->
            a.x < b.x
        )
        viewState.aggEdge.aAddress
        viewState.aggEdge.bAddress
        |> Maybe.withDefault True


view : View.Config -> Pathfinder.Model -> ( Id, Id ) -> RelationDetails.Model -> Html Msg
view vc model id viewState =
    let
        network =
            id |> first |> Id.network

        ( ( right, left ), ( isA2b, isB2a ), ( leftId, rightId ) ) =
            if isLeftToRight viewState then
                ( ( viewState.aggEdge.a2b, viewState.aggEdge.b2a )
                , ( True, False )
                , ( viewState.aggEdge.a, viewState.aggEdge.b )
                )

            else
                ( ( viewState.aggEdge.b2a, viewState.aggEdge.a2b )
                , ( False, True )
                , ( viewState.aggEdge.b, viewState.aggEdge.a )
                )

        valuesList =
            makeValuesList vc
                network
                (RemoteData.withDefault Nothing right)
                (RemoteData.withDefault Nothing left)

        valuesToValuesRow { leftValue, rightValue } =
            SidePanelComponents.sidePanelRelationshipValuesRowWithInstances
                (SidePanelComponents.sidePanelRelationshipValuesRowAttributes
                    |> Rs.s_root [ spread ]
                )
                (SidePanelComponents.sidePanelRelationshipValuesRowInstances
                    |> Rs.s_leftValue
                        (if RemoteData.isLoading viewState.aggEdge.b2a then
                            loadingSpinner vc Css.View.loadingSpinner
                                |> Just

                         else
                            Nothing
                        )
                    |> Rs.s_rightValue
                        (if RemoteData.isLoading viewState.aggEdge.a2b then
                            loadingSpinner vc Css.View.loadingSpinner
                                |> Just

                         else
                            Nothing
                        )
                )
                { leftValue =
                    { firstRowText = leftValue.coin
                    , secondRowText = leftValue.fiat
                    , secondRowVisible = True
                    }
                , rightValue =
                    { firstRowText = rightValue.coin
                    , secondRowText = rightValue.fiat
                    , secondRowVisible = True
                    }
                }
    in
    div []
        (SidePanelComponents.sidePanelRelationshipWithAttributes
            (SidePanelComponents.sidePanelRelationshipAttributes
                |> Rs.s_root
                    [ sidePanelCss
                        |> css
                    ]
                |> Rs.s_iconsCloseBlack (closeAttrs UserClosedDetailsView)
                |> Rs.s_valuesList
                    [ css [ Css.overflowY Css.auto ] ]
            )
            { tabsList =
                [ tableTab vc model.network id viewState isA2b
                , tableTab vc model.network id viewState isB2a
                ]
                    |> List.map (Html.Styled.map (RelationDetailsMsg id))
            , valuesList =
                valuesList
                    |> List.map valuesToValuesRow
            }
            { leftTab = { variant = none }
            , rightTab = { variant = none }
            , root =
                { tabsVisible = False
                , address1 =
                    leftId
                        |> Id.id
                        |> truncateLongIdentifier
                , address2 =
                    rightId
                        |> Id.id
                        |> truncateLongIdentifier
                , title =
                    Locale.string vc.locale "Asset transfers between"
                        |> Locale.titleCase vc.locale
                , totalReceivedLabel = Locale.string vc.locale "Total transferred"
                }
            }
            :: ([ ( True, viewState.a2bTable ), ( False, viewState.b2aTable ) ]
                    |> List.map
                        (\( isA2b_, ts ) ->
                            if ts.isTxFilterViewOpen then
                                let
                                    filterDialogMsgs =
                                        { closeTxFilterViewMsg = RelationDetails.CloseTxFilterView isA2b_
                                        , txTableFilterShowAllTxsMsg = Nothing
                                        , txTableFilterShowIncomingTxOnlyMsg = Nothing
                                        , txTableFilterShowOutgoingTxOnlyMsg = Nothing
                                        , resetAllTxFiltersMsg = RelationDetails.ResetAllTxFilters isA2b_
                                        , txTableAssetSelectBoxMsg = RelationDetails.TxTableAssetSelectBoxMsg isA2b_
                                        , openDateRangePickerMsg = Just (RelationDetails.OpenDateRangePicker isA2b_)
                                        , txTableFilterToggleZeroValueMsg = Nothing
                                        }
                                in
                                div
                                    [ [ Css.position Css.fixed
                                      , Css.right (Css.px 42)
                                      , Css.top (Css.px 350)
                                      , Css.property "transform" "translate(0%, -50%)"
                                      , Css.zIndex (Css.int (Util.Css.zIndexMainValue + 1000))
                                      ]
                                        |> css
                                    ]
                                    [ TransactionFilter.txFilterDialogView vc network filterDialogMsgs ts |> Html.Styled.map (Pathfinder.RelationDetailsMsg id) ]

                            else
                                none
                        )
               )
        )


tableTab : View.Config -> Network -> ( Id, Id ) -> RelationDetails.Model -> Bool -> Html RelationDetails.Msg
tableTab vc network edgeId viewState isA2b =
    let
        { open, table, id, relation } =
            if isA2b then
                { open = viewState.a2bTableOpen
                , table = viewState.a2bTable
                , id = first edgeId
                , relation = viewState.aggEdge.a2b
                }

            else
                { open = viewState.b2aTableOpen
                , table = viewState.b2aTable
                , id = second edgeId
                , relation = viewState.aggEdge.b2a
                }

        noAddresses =
            relation
                |> RemoteData.toMaybe
                |> Maybe.withDefault Nothing
                |> Maybe.map .noTxs
                |> Maybe.withDefault 0

        arrow =
            case ( isLeftToRight viewState, isA2b ) of
                ( True, True ) ->
                    Icons.iconsArrowRightThin {}

                ( True, False ) ->
                    Icons.iconsArrowLeftThin {}

                ( False, True ) ->
                    Icons.iconsArrowLeftThin {}

                ( False, False ) ->
                    Icons.iconsArrowRightThin {}
    in
    dataTab
        { title =
            SidePanelComponents.sidePanelListHeaderTitleRelationWithInstances
                (SidePanelComponents.sidePanelListHeaderTitleRelationAttributes
                    |> Rs.s_root [ spread ]
                    |> Rs.s_valueFrame [ [ Css.width Css.auto ] |> css ]
                )
                (SidePanelComponents.sidePanelListHeaderTitleRelationInstances
                    |> Rs.s_totalNumber
                        (if RemoteData.isLoading relation then
                            loadingSpinner vc Css.View.loadingSpinner |> Just

                         else
                            Nothing
                        )
                )
                { root =
                    { fromText = Locale.string vc.locale "From"
                    , address = Id.id id |> truncateLongIdentifier
                    , iconInstance = arrow
                    , number =
                        case relation of
                            Loading ->
                                ""

                            NotAsked ->
                                ""

                            Failure _ ->
                                "error"

                            Success no ->
                                no
                                    |> Maybe.map (.noTxs >> Locale.int vc.locale)
                                    |> Maybe.withDefault "0"
                    }
                }
        , disabled = noAddresses == 0
        , content =
            if not open || noAddresses == 0 then
                Nothing

            else
                let
                    allChecked =
                        table.table
                            |> Components.InfiniteTable.getPage
                            |> List.map Tx.getTxIdForRelationTx
                            |> allAndNotEmpty isChecked

                    isChecked =
                        flip Network.hasTx network

                    conf =
                        { isChecked = isChecked
                        , allChecked = allChecked
                        , addressId = id
                        , isA2b = isA2b
                        }

                    tableView =
                        InfiniteTable.view vc
                            [ css fullWidth ]
                            (RelationTxsTable.config Css.Table.styles vc conf)
                            table.table
                in
                div
                    [ css <|
                        SidePanelComponents.sidePanelRelatedAddressesContent_details.styles
                            ++ fullWidth
                    ]
                    [ TransactionFilter.filterHeader vc
                        table
                        { resetDateFilterMsg = RelationDetails.ResetDateRangePicker isA2b
                        , resetAssetsFilterMsg = RelationDetails.ResetTxAssetFilter isA2b
                        , resetDirectionFilterMsg = Nothing
                        , toggleFilterView = RelationDetails.ToggleTxFilterView isA2b
                        , resetZeroValueFilterMsg = Nothing
                        , exportCsv = Nothing
                        }
                    , tableView
                    ]
                    |> Just
        , onClick = RelationDetails.UserClickedToggleTable isA2b
        }


type alias ValuesRow =
    { leftValue : ValuesFormatted
    , rightValue : ValuesFormatted
    }


type alias ValuesFormatted =
    { fiat : String
    , fiatFloat : Float
    , coin : String
    , value : Int
    , asset : AssetIdentifier
    }


makeValuesList : View.Config -> String -> Maybe Api.Data.NeighborAddress -> Maybe Api.Data.NeighborAddress -> List ValuesRow
makeValuesList vc network right left =
    let
        leftValues =
            left
                |> relationToValues

        rightValues =
            right
                |> relationToValues

        getValue ( asset, values ) =
            let
                fiatCurr =
                    vc.preferredFiatCurrency

                ass =
                    Currency.asset network asset

                coin =
                    Locale.coin vc.locale ass values.value

                fvalue =
                    Locale.getFiatValue fiatCurr values
                        |> Maybe.withDefault 0
            in
            { fiat =
                fvalue
                    |> Locale.fiat vc.locale fiatCurr
            , fiatFloat = fvalue
            , coin = coin
            , value = values.value
            , asset = ass
            }
                |> pair asset

        emptyValues asset =
            { fiat = Locale.fiat vc.locale vc.preferredFiatCurrency 0
            , fiatFloat = 0
            , coin = Locale.coin vc.locale (Currency.asset network asset) 0
            , value = 0
            , asset = Currency.asset network asset
            }

        relationToValues =
            Maybe.map
                (\{ value, tokenValues } ->
                    getValue ( network, value )
                        |> flip (::)
                            (tokenValues
                                |> Maybe.withDefault Dict.empty
                                |> Dict.toList
                                |> List.map getValue
                            )
                        |> Dict.fromList
                )
                >> Maybe.withDefault Dict.empty

        sort { rightValue, leftValue } =
            rightValue.fiatFloat + leftValue.fiatFloat

        leftStep asset values =
            Dict.insert
                asset
                { leftValue = values
                , rightValue = emptyValues asset
                }

        rightStep asset values =
            Dict.insert
                asset
                { leftValue = emptyValues asset
                , rightValue = values
                }

        bothStep asset lv rv =
            Dict.insert
                asset
                { leftValue = lv
                , rightValue = rv
                }
    in
    Dict.merge
        leftStep
        bothStep
        rightStep
        leftValues
        rightValues
        Dict.empty
        |> Dict.values
        |> List.sortBy sort
        |> List.reverse
