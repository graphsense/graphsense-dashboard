module View.Pathfinder.RelationDetails exposing (view)

import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth, sidePanelCss)
import Css.Table
import Css.View
import Html.Styled exposing (Html, div)
import Maybe.Extra
import Model.Currency exposing (assetFromBase)
import Model.Locale as Locale
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.RelationDetails as RelationDetails
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder as Pathfinder exposing (Msg(..))
import Msg.Pathfinder.RelationDetails as RelationDetails
import PagedTable
import RecordSetter as Rs
import RemoteData exposing (RemoteData(..))
import Svg.Styled.Attributes exposing (css)
import Theme.Html.Icons as Icons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Tuple exposing (first, second)
import Util exposing (allAndNotEmpty)
import Util.Css exposing (spread)
import Util.View exposing (loadingSpinner, none, truncateLongIdentifier)
import View.Locale as Locale
import View.Pathfinder.Details exposing (closeAttrs, dataTab)
import View.Pathfinder.PagedTable as PagedTable
import View.Pathfinder.Table.RelationTxsTable as RelationTxsTable
import View.Pathfinder.TransactionFilter as TransactionFilter


view : View.Config -> Pathfinder.Model -> ( Id, Id ) -> RelationDetails.Model -> Html Msg
view vc model id viewState =
    let
        fiatValue v =
            v.value
                |> Locale.getFiatValue vc.preferredFiatCurrency
                |> Maybe.map (Locale.fiat vc.locale vc.preferredFiatCurrency)
                |> Maybe.withDefault ""

        asset =
            id
                |> first
                |> Id.network
                |> assetFromBase

        network =
            id |> first |> Id.network

        cryptoValue v =
            v.value.value
                |> Locale.coin vc.locale asset
    in
    div []
        (SidePanelComponents.sidePanelRelationshipWithInstances
            (SidePanelComponents.sidePanelRelationshipAttributes
                |> Rs.s_root
                    [ sidePanelCss
                        |> css
                    ]
                |> Rs.s_iconsCloseBlack closeAttrs
            )
            (SidePanelComponents.sidePanelRelationshipInstances
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
            { tabsList =
                [ tableTab vc model.network id viewState True
                , tableTab vc model.network id viewState False
                ]
                    |> List.map (Html.Styled.map (RelationDetailsMsg id))
            }
            { leftTab = { variant = none }
            , rightTab = { variant = none }
            , title = { infoLabel = Locale.string vc.locale "Total received" }
            , root =
                { tabsVisible = False
                , address1 =
                    viewState.aggEdge.a
                        |> Id.id
                        |> truncateLongIdentifier
                , address2 =
                    viewState.aggEdge.b
                        |> Id.id
                        |> truncateLongIdentifier
                , title = Locale.string vc.locale "Asset transfers between"
                }
            , leftValue =
                { firstRowText =
                    viewState.aggEdge.b2a
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
                        |> Maybe.map cryptoValue
                        |> Maybe.withDefault "0"
                , secondRowText =
                    viewState.aggEdge.b2a
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
                        |> Maybe.map fiatValue
                        |> Maybe.withDefault "0"
                , secondRowVisible = True
                }
            , rightValue =
                { firstRowText =
                    viewState.aggEdge.a2b
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
                        |> Maybe.map cryptoValue
                        |> Maybe.withDefault "0"
                , secondRowText =
                    viewState.aggEdge.a2b
                        |> RemoteData.toMaybe
                        |> Maybe.Extra.join
                        |> Maybe.map fiatValue
                        |> Maybe.withDefault "0"
                , secondRowVisible = True
                }
            }
            :: ([ ( True, viewState.a2bTable ), ( False, viewState.b2aTable ) ]
                    |> List.map
                        (\( isA2b, ts ) ->
                            if ts.isTxFilterViewOpen then
                                let
                                    filterDialogMsgs =
                                        { closeTxFilterViewMsg = RelationDetails.CloseTxFilterView isA2b
                                        , txTableFilterShowAllTxsMsg = Nothing
                                        , txTableFilterShowIncomingTxOnlyMsg = Nothing
                                        , txTableFilterShowOutgoingTxOnlyMsg = Nothing
                                        , resetAllTxFiltersMsg = RelationDetails.ResetAllTxFilters isA2b
                                        , txTableAssetSelectBoxMsg = RelationDetails.TxTableAssetSelectBoxMsg isA2b
                                        , openDateRangePickerMsg = RelationDetails.OpenDateRangePicker isA2b
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
        { open, table, id, address } =
            if isA2b then
                { open = viewState.a2bTableOpen
                , table = viewState.a2bTable
                , id = first edgeId
                , address = viewState.aggEdge.a2b
                }

            else
                { open = viewState.b2aTableOpen
                , table = viewState.b2aTable
                , id = second edgeId
                , address = viewState.aggEdge.b2a
                }

        noAddresses =
            address
                |> RemoteData.toMaybe
                |> Maybe.Extra.join
                |> Maybe.map .noTxs
    in
    dataTab
        { title =
            SidePanelComponents.sidePanelListHeaderTitleRelationWithInstances
                (SidePanelComponents.sidePanelListHeaderTitleRelationAttributes
                    |> Rs.s_root [ spread ]
                )
                (SidePanelComponents.sidePanelListHeaderTitleRelationInstances
                    |> Rs.s_totalNumber
                        (if RemoteData.isLoading address then
                            loadingSpinner vc Css.View.loadingSpinner |> Just

                         else
                            Nothing
                        )
                )
                { root =
                    { fromText = Locale.string vc.locale "From"
                    , address = Id.id id |> truncateLongIdentifier
                    , iconInstance =
                        if isA2b then
                            Icons.iconsArrowRightThin {}

                        else
                            Icons.iconsArrowLeftThin {}
                    , number =
                        case address of
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
        , disabled = noAddresses == Nothing || noAddresses == Just 0
        , content =
            if not open || noAddresses == Nothing || noAddresses == Just 0 then
                Nothing

            else
                let
                    allChecked =
                        table.table
                            |> PagedTable.getPage
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
                        PagedTable.pagedTableView vc
                            [ css fullWidth ]
                            (RelationTxsTable.config Css.Table.styles vc conf)
                            table.table
                            (RelationDetails.TableMsg isA2b)
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
                        }
                    , tableView
                    ]
                    |> Just
        , onClick = RelationDetails.UserClickedToggleTable isA2b
        }
