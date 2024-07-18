module View.Pathfinder exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Browser.Events exposing (Visibility(..))
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Graph
import Css.Pathfinder as Css exposing (..)
import Css.Table
import Css.View
import Dict
import DurationDatePicker as DatePicker
import FontAwesome
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (id, src)
import Html.Styled.Lazy exposing (..)
import Json.Decode
import Model.Currency exposing (assetFromBase)
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (BBox, Coords)
import Model.Graph.Table
import Model.Graph.Transform exposing (Transition(..))
import Model.Pathfinder exposing (..)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Model.Pathfinder.Tx as Tx
import Model.Pathfinder.TxDetails as TxDetails
import Msg.Pathfinder exposing (DisplaySettingsMsg(..), Msg(..), TxDetailsMsg(..))
import Msg.Pathfinder.AddressDetails as AddressDetails
import Number.Bounded exposing (value)
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter exposing (s_headRow, s_root)
import RemoteData
import Route.Pathfinder exposing (Route(..))
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as SA exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Lazy as Svg
import Table
import Theme.Html.Icons
import Theme.Html.SidebarComponents as SidebarComponents
import Theme.Svg.Icons as Icons
import Update.Graph.Transform as Transform
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.Graph
import Util.View exposing (copyIcon, copyableLongIdentifierPathfinder, none, truncateLongIdentifierWithLengths)
import View.Graph.Table exposing (noTools)
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.Icons exposing (inIcon, outIcon)
import View.Pathfinder.Network as Network
import View.Pathfinder.Table as Table
import View.Pathfinder.Table.IoTable as IoTable
import View.Pathfinder.Table.NeighborsTable as NeighborsTable
import View.Pathfinder.Table.TransactionTable as TransactionTable
import View.Pathfinder.Tooltip as Tooltip
import View.Search
import Hex


type alias BtnConfig =
    { icon : FontAwesome.Icon, text : String, msg : Msg, enable : Bool }


graphActionTools : Model -> List BtnConfig
graphActionTools m =
    [ BtnConfig FontAwesome.file "restart" UserClickedRestart m.isDirty
    , BtnConfig FontAwesome.folder "open" NoOp False
    , BtnConfig FontAwesome.redo "redo" UserClickedRedo (not (List.isEmpty m.history.future))
    , BtnConfig FontAwesome.undo "undo" UserClickedUndo (not (List.isEmpty m.history.past))
    ]


graphActionButtons : List BtnConfig
graphActionButtons =
    [ BtnConfig FontAwesome.arrowUp "Import file" UserClickedImportFile True
    , BtnConfig FontAwesome.download "Export" (UserClickedExportGraphAsPNG "graph") True
    ]



-- Helpers


type ValueType
    = ValueInt Int
    | ValueHex Int
    | Text String
    | Currency Api.Data.Values String
    | CurrencyWithCode Api.Data.Values String
    | InOut (Maybe Int) Int Int
    | Timestamp Int
    | TimestampWithTime Int
    | CopyIdent String


type KVTableRow
    = Row String ValueType
    | Gap



-- http://probablyprogramming.com/2009/03/15/the-tiniest-gif-ever


renderKVTable : View.Config -> List KVTableRow -> Html Msg
renderKVTable vc rows =
    table [ fullWidth |> toAttr ] (rows |> List.map (renderKVRow vc))


renderKVRow : View.Config -> KVTableRow -> Html Msg
renderKVRow vc row =
    case row of
        Row key value ->
            tr []
                [ td [ kVTableKeyTdStyle vc |> toAttr ] [ Html.text (Locale.string vc.locale key) ]
                , td [ kVTableValueTdStyle vc |> toAttr ] (renderValueTypeValue vc value |> List.singleton)
                , td [ kVTableTdStyle vc |> toAttr ] (renderValueTypeExtension vc value |> List.singleton)
                ]

        Gap ->
            tr []
                [ td [ kVTableKeyTdStyle vc |> toAttr ] []
                , td [] []
                , td [] []
                ]


renderValueTypeValue : View.Config -> ValueType -> Html Msg
renderValueTypeValue vc val =
    case val of
        ValueInt v ->
            span [] [ Html.text (String.fromInt v) ]
        
        ValueHex v ->
            span [] [ Html.text (Hex.toString v) ]

        InOut total inv outv ->
            inOutIndicator total inv outv

        Text txt ->
            span [] [ Html.text txt ]

        Currency v ticker ->
            span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.coinWithoutCode vc.locale (assetFromBase ticker) v.value) ]

        CurrencyWithCode v ticker ->
            span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.coinWithoutCode vc.locale (assetFromBase ticker) v.value ++ " " ++ ticker) ]

        CopyIdent ident ->
            Util.View.copyableLongIdentifierPathfinder vc [] ident

        Timestamp ts ->
            span [] [ Locale.date vc.locale ts |> Html.text ]

        TimestampWithTime ts ->
            span [] [ Locale.time vc.locale ts |> Html.text ]


renderValueTypeExtension : View.Config -> ValueType -> Html Msg
renderValueTypeExtension _ val =
    case val of
        Currency _ ticker ->
            span [] [ Html.text (String.toUpper ticker) ]

        _ ->
            none


disableableButton : (Bool -> List Css.Style) -> BtnConfig -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
disableableButton style btn attrs content =
    let
        addattr =
            if btn.enable then
                [ btn.msg |> onClick ]

            else
                [ HA.disabled True ]
    in
    button (((style btn.enable |> toAttr) :: addattr) ++ attrs) content


rule : Html Msg
rule =
    hr [ ruleStyle |> toAttr ] []


inOutIndicator : Maybe Int -> Int -> Int -> Html Msg
inOutIndicator mnr inNr outNr =
    let
        prefix =
            String.trim (String.join " " [ mnr |> Maybe.map String.fromInt |> Maybe.withDefault "", "(" ])
    in
    span [ ioOutIndicatorStyle |> toAttr ] [ Html.text prefix, inIcon, Html.text (String.fromInt inNr), Html.text ",", outIcon, Html.text (String.fromInt outNr), Html.text ")" ]


collapsibleSection : View.Config -> String -> Bool -> Maybe (Html Msg) -> Html Msg -> Msg -> Html Msg
collapsibleSection vc =
    collapsibleSectionRaw (collapsibleSectionHeadingStyle vc |> toAttr) (collapsibleSectionIconStyle |> toAttr) vc


collapsibleSectionRaw : Html.Attribute Msg -> Html.Attribute Msg -> View.Config -> String -> Bool -> Maybe (Html Msg) -> Html Msg -> Msg -> Html Msg
collapsibleSectionRaw headingAttr iconAttr vc title open indicator content action =
    let
        icon =
            if open then
                FontAwesome.chevronDown

            else
                FontAwesome.chevronRight

        data =
            if open then
                [ content ]

            else
                []
    in
    div []
        (div [ headingAttr, onClick action ]
            [ span [ iconAttr ] [ FontAwesome.icon icon |> Html.fromUnstyled ]
            , Html.text (Locale.string vc.locale title)
            , indicator |> Maybe.withDefault none
            ]
            :: data
        )



-- View


view : Plugins -> ModelState -> View.Config -> Model -> { navbar : List (Html Msg), contents : List (Html Msg) }
view plugins states vc model =
    { navbar = []
    , contents = graph plugins states vc model.config model
    }


graph : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> List (Html Msg)
graph plugins states vc gc model =
    [ vc.size
        |> Maybe.map (graphSvg plugins states vc gc model)
        |> Maybe.withDefault none
    , topLeftPanel plugins states vc gc model
    , topRightPanel plugins states vc gc model
    , graphSelectionToolsView plugins states vc gc model
    ]
        ++ (model.tooltip
                |> Maybe.map (Tooltip.view vc model.tagSummaries)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
           )


topLeftPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
topLeftPanel plugins ms vc gc model =
    div [ topLeftPanelStyle vc |> toAttr ]
        [ h2 [ vc.theme.heading2 |> toAttr ] [ Html.text "Pathfinder" ]
        , graphActionsTopLeftView plugins ms vc gc model
        , searchBoxView plugins ms vc gc model
        , settingsView vc gc model
        ]


settingsView : View.Config -> Pathfinder.Config -> Model -> Html Msg
settingsView vc pc m =
    let
        content =
            div []
                [ span [ panelHeadingStyle3 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Transaction Settings") ]
                , Util.View.onOffSwitch vc [ HA.checked pc.showTxTimestamps, onClick (UserClickedToggleShowTxTimestamp |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "Show timestamp")
                , span [ panelHeadingStyle3 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Date Settings") ]
                , Util.View.onOffSwitch vc [ HA.checked vc.showDatesInUserLocale, onClick (UserClickedToggleDatesInUserLocale |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "UTC")
                ]
    in
    div [ boxStyle vc Nothing |> toAttr ]
        [ collapsibleSectionRaw (collapsibleSectionHeadingDisplaySettingsStyle vc |> toAttr) (collapsibleSectionDisplaySettingsIconStyle |> toAttr) vc "Display" m.config.isDisplaySettingsOpen Nothing content (ChangedDisplaySettingsMsg UserClickedToggleDisplaySettings)
        ]


graphActionsTopLeftView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
graphActionsTopLeftView _ _ vc _ m =
    div
        [ graphActionsStyle vc |> toAttr
        ]
        (graphActionTools m |> List.map (graphToolButton vc))


graphSelectionToolsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
graphSelectionToolsView _ _ vc _ m =
    let
        selectBtn =
            BtnConfig FontAwesome.mousePointer "select" (ChangePointerTool Select |> ChangedDisplaySettingsMsg) True

        dragBtn =
            BtnConfig FontAwesome.handPaper "Drag" (ChangePointerTool Drag |> ChangedDisplaySettingsMsg) True
    in
    div
        [ graphSelectionToolsStyle vc |> toAttr
        ]
        [ graphSelectionToolButton vc selectBtn (m.pointerTool == Select)
        , graphSelectionToolButton vc dragBtn (m.pointerTool == Drag)
        ]


graphSelectionToolButton : View.Config -> BtnConfig -> Bool -> Svg Msg
graphSelectionToolButton vc btn selected =
    div [ toolItemSmallStyle vc selected |> toAttr ]
        [ disableableButton (toggleToolButtonStyle vc selected)
            btn
            [ HA.title (Locale.string vc.locale btn.text) ]
            [ div [ toolIconStyle vc |> toAttr ] [ FontAwesome.icon btn.icon |> Html.fromUnstyled ]
            ]
        ]


graphToolButton : View.Config -> BtnConfig -> Svg Msg
graphToolButton vc btn =
    div [ toolItemStyle vc |> toAttr ]
        [ disableableButton (toolButtonStyle vc)
            btn
            []
            [ div [ toolIconStyle vc |> toAttr ] [ FontAwesome.icon btn.icon |> Html.fromUnstyled ]
            , Html.text (Locale.string vc.locale btn.text)
            ]
        ]


topRightPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
topRightPanel _ _ vc gc model =
    div [ topRightPanelStyle vc |> toAttr ]
        [ graphActionsView vc gc model
        , detailsView vc gc model
        ]


graphActionsView : View.Config -> Pathfinder.Config -> Model -> Html Msg
graphActionsView vc _ _ =
    div [ graphActionsViewStyle vc |> toAttr ]
        (graphActionButtons |> List.map (graphActionButton vc))


graphActionButton : View.Config -> BtnConfig -> Html Msg
graphActionButton vc btn =
    disableableButton (graphActionButtonStyle vc) btn [] (iconWithText vc btn.icon (Locale.string vc.locale btn.text))


iconWithText : View.Config -> FontAwesome.Icon -> String -> List (Html Msg)
iconWithText _ faIcon text =
    [ span [ iconWithTextStyle |> toAttr ] [ FontAwesome.icon faIcon |> Html.fromUnstyled ], Html.text text ]


searchBoxView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
searchBoxView plugins _ vc _ model =
    div
        [ searchBoxStyle vc Nothing |> toAttr ]
        [ span [ panelHeadingStyle2 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Search") ]
        , div [ searchBoxContainerStyle vc |> toAttr ]
            [ span [ searchBoxIconStyle vc |> toAttr ] [ FontAwesome.icon FontAwesome.search |> Html.fromUnstyled ]
            , View.Search.search plugins
                vc
                { css = searchInputStyle vc
                , multiline = False
                , resultsAsLink = True
                , showIcon = False
                }
                model.search
                |> Html.map SearchMsg
            ]
        ]


detailsView : View.Config -> Pathfinder.Config -> Model -> Html Msg
detailsView vc gc model =
    case model.details of
        Just details ->
            div
                [ detailsViewStyle vc |> toAttr ]
                [ detailsViewCloseRow vc
                , case details of
                    AddressDetails id state ->
                        RemoteData.unwrap
                            (Util.View.loadingSpinner vc Css.View.loadingSpinner)
                            (addressDetailsContentView vc gc model id)
                            state

                    TxDetails id state ->
                        txDetailsContentView vc gc model id state
                ]

        Nothing ->
            none


detailsViewCloseRow : View.Config -> Html Msg
detailsViewCloseRow vc =
    div [ detailsViewCloseButtonStyle |> toAttr ] [ closeButton vc UserClosedDetailsView ]


closeButton : View.Config -> Msg -> Html Msg
closeButton vc msg =
    button [ linkButtonStyle vc True |> toAttr, msg |> onClick ] [ FontAwesome.icon FontAwesome.times |> Html.fromUnstyled ]


getAddressAnnotationBtns : View.Config -> Api.Data.Address -> Maybe Api.Data.Actor -> Bool -> List BtnConfig
getAddressAnnotationBtns vc data actor hasTags =
    let
        isContract x =
            x.isContract |> Maybe.withDefault False
    in
    -- (if hasTags then
    --     [ BtnConfig FontAwesome.tags (Locale.string vc.locale "has tags") NoOp True ]
    --  else
    --     []
    -- )
    if isContract data then
        [ BtnConfig FontAwesome.cog (Locale.string vc.locale "is contract") NoOp True ]

    else
        []



-- ++ (actor |> Maybe.map (\a -> [ BtnConfig FontAwesome.user a.label NoOp True ]) |> Maybe.withDefault [])


getAddressActionBtns : Id -> Api.Data.Address -> List BtnConfig
getAddressActionBtns _ _ =
    []



-- [ BtnConfig FontAwesome.tags "Remove from Graph" (UserClickedRemoveAddressFromGraph id) True ]


txDetailsContentView : View.Config -> Pathfinder.Config -> Model -> Id -> TxDetails.Model -> Html Msg
txDetailsContentView vc gc model id viewState =
    let
        header =
            [ longIdentDetailsHeadingView vc gc id "Transaction" []
            , rule
            ]

        getLbl id_ =
            Dict.get id_ model.tagSummaries
                |> Maybe.withDefault NoTags

        ( detailsTblBody, sections ) =
            case viewState.tx.type_ of
                Tx.Account tx ->
                    ( [ accountTxDetailsContentView vc tx.raw ]
                    , [ none ]
                    )

                Tx.Utxo tx ->
                    ( [ utxoTxDetailsContentView vc tx.raw ]
                    , [ utxoTxDetailsSectionsView vc model.network viewState tx.raw getLbl ]
                    )
    in
    div []
        (div [ detailsContainerStyle |> toAttr ]
            [ div [ detailsViewContainerStyle vc |> toAttr ]
                [ div [ fullWidth |> toAttr ] (header ++ detailsTblBody)
                ]
            ]
            :: sections
        )


utxoTxDetailsContentView : View.Config -> Api.Data.TxUtxo -> Html Msg
utxoTxDetailsContentView vc data =
    let
        actionBtns =
            []

        -- [ BtnConfig FontAwesome.tags "Do it" NoOp True ]
        tbls =
            [ detailsFactTableView vc (apiUtxoTxToRows data), detailsActionsView vc actionBtns ]
    in
    div [] tbls


ioTableView : View.Config -> Network -> String -> Model.Graph.Table.Table Api.Data.TxValue -> (Id -> HavingTags) -> Html Msg
ioTableView vc network currency table getLbl =
    let
        isCheckedFn =
            flip Network.hasAddress network

        styles =
            Css.Table.styles
                |> s_root (\vc_ -> Css.Table.styles.root vc_ ++ [ Css.display Css.block ])
                |> s_headRow (\vc_ -> Css.Table.styles.headRow vc_ ++ [ Css.textAlign Css.right ])
    in
    View.Graph.Table.table
        styles
        vc
        [ css [ Css.overflowY Css.auto, Css.maxHeight (Css.px ((vc.size |> Maybe.map .height |> Maybe.withDefault 500) * 0.5)) ] ]
        noTools
        (IoTable.config styles vc currency isCheckedFn (Just getLbl))
        table


utxoTxDetailsSectionsView : View.Config -> Network -> TxDetails.Model -> Api.Data.TxUtxo -> (Id -> HavingTags) -> Html Msg
utxoTxDetailsSectionsView vc network viewState data getLbl =
    let
        content =
            ioTableView vc network data.currency viewState.table getLbl

        ioIndicatorState =
            Just (inOutIndicator Nothing data.noOutputs data.noInputs)
    in
    collapsibleSection vc "In- and Outputs" viewState.ioTableOpen ioIndicatorState content (TxDetailsMsg UserClickedToggleIOTable)


accountTxDetailsContentView : View.Config -> Api.Data.TxAccount -> Html Msg
accountTxDetailsContentView _ _ =
    div [] [ Html.text "I am a Account TX" ]


addressDetailsContentView : View.Config -> Pathfinder.Config -> Model -> Id -> AddressDetails.Model -> Html Msg
addressDetailsContentView vc gc model id viewState =
    let
        address =
            model.network.addresses
                |> Dict.get id
                |> Maybe.withDefault viewState.address

        ts =
            case Dict.get id model.tagSummaries of
                Just (HasTagSummary t) ->
                    Just t

                _ ->
                    Nothing

        tags =
            ts |> Maybe.map (.labelTagCloud >> Dict.toList >> List.sortBy (Tuple.second >> .weighted)) |> Maybe.withDefault []

        tagsDisplay =
            tags |> List.reverse |> List.take 2 |> List.map Tuple.first

        tagsDisplayWithMore =
            tagsDisplay
                ++ (if List.length tags > List.length tagsDisplay then
                        [ "..." ]

                    else
                        []
                   )

        actor_id =
            ts |> Maybe.andThen .bestActor

        actor =
            actor_id
                |> Maybe.andThen (\i -> Dict.get i model.actors)

        actorImg =
            actor
                |> Maybe.andThen .context
                |> Maybe.andThen (.images >> List.head)
                |> Maybe.map addProtocolPrefx

        actorText =
            actor
                |> Maybe.map .label

        txOnGraphFn =
            \txId -> Dict.member txId model.network.txs

        sections =
            [ addressTransactionTableView vc gc id viewState txOnGraphFn

            -- , addressNeighborsTableView vc gc id viewState viewState.data
            ]

        tbls =
            [ detailsFactTableView vc (apiAddressToRows viewState.data), detailsActionsView vc (getAddressActionBtns id viewState.data) ]

        -- addressAnnotationBtns =
        --     getAddressAnnotationBtns vc viewState.data actor (Dict.member id model.tagSummaries)
        df =
            SidebarComponents.sidePanelHeaderComponentAttributes

        inst =
            SidebarComponents.sidePanelHeaderComponentInstances

        showExchangeTag =
            actorText /= Nothing

        showOtherTag =
            List.isEmpty tags |> not
    in
    SidebarComponents.sidePanelHeaderComponentWithInstances
        { df
            | exchangeLabelOf8 =
                [ css
                    [ Css.whiteSpace Css.noWrap
                    ]
                ]
        }
        { inst
            | sidePanelHeaderTags =
                if showExchangeTag || showOtherTag then
                    Nothing

                else
                    Just none
        }
        { sidePanelHeaderMain =
            { header = (String.toUpper <| Id.network id) ++ " " ++ Locale.string vc.locale "address"
            , icon =
                if address.exchange /= Nothing then
                    Icons.iconsExchangeSvg [] {}

                else
                    Icons.iconsUntaggedSvg [] {}
            }
        , sidePanelHeaderTags =
            { exchangeTag = showExchangeTag
            , otherTag = showOtherTag
            }
        , iconTextOf16 =
            { icon = Icons.iconsTagSvg [] {}
            , text = String.join ", " tagsDisplayWithMore
            }
        , iconTextOf6 =
            { icon = Id.id id |> copyIcon vc
            , text = Id.id id |> truncateLongIdentifierWithLengths 13 13
            }
        , iconTextOf12 =
            { icon =
                actorImg
                    |> Maybe.map
                        (\imgSrc ->
                            img
                                [ src imgSrc
                                , HA.alt <| Maybe.withDefault "" <| actorText
                                , HA.width <| round Icons.iconsTagTagIconDetails.width
                                , HA.height <| round Icons.iconsTagTagIconDetails.height
                                , HA.css Theme.Html.Icons.iconsTagTagIconDetails.styles
                                ]
                                []
                                |> List.singleton
                                |> div
                                    [ HA.css Theme.Html.Icons.iconsTagDetails.styles
                                    , HA.css
                                        [ Theme.Html.Icons.iconsTagDetails.width
                                            |> Css.px
                                            |> Css.width
                                        , Theme.Html.Icons.iconsTagDetails.height
                                            |> Css.px
                                            |> Css.height
                                        ]
                                    ]
                        )
                    |> Maybe.withDefault (Icons.iconsTagSvg [] {})
            , text = actorText |> Maybe.withDefault ""
            }
        }
        :: tbls
        |> div [ detailsContainerStyle |> toAttr ]
        |> flip (::) sections
        |> div []


addressTransactionTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetails.Model -> (Id -> Bool) -> Html Msg
addressTransactionTableView vc _ _ viewState txOnGraphFn =
    let
        data =
            viewState.data

        content =
            transactionTableView vc data.currency txOnGraphFn viewState.txs

        ioIndicatorState =
            Just (inOutIndicator (Just (data.noIncomingTxs + data.noOutgoingTxs)) data.noIncomingTxs data.noOutgoingTxs)
    in
    collapsibleSection vc "Transactions" viewState.transactionsTableOpen ioIndicatorState content (AddressDetailsMsg AddressDetails.UserClickedToggleTransactionTable)


addressNeighborsTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetails.Model -> Api.Data.Address -> Html Msg
addressNeighborsTableView vc _ _ viewState data =
    let
        attributes =
            []

        prevMsg =
            \dir _ -> AddressDetailsMsg (AddressDetails.UserClickedPreviousPageNeighborsTable dir)

        nextMsg =
            \dir _ -> AddressDetailsMsg (AddressDetails.UserClickedNextPageNeighborsTable dir)

        tblCfg =
            NeighborsTable.config vc data.currency

        content =
            div []
                [ h2 [ panelHeadingStyle2 vc |> toAttr ] [ Html.text "Outgoing" ]
                , Table.pagedTableView vc attributes tblCfg viewState.neighborsOutgoing (prevMsg Outgoing) (nextMsg Outgoing)
                , h2 [ panelHeadingStyle2 vc |> toAttr ] [ Html.text "Incoming" ]
                , Table.pagedTableView vc attributes tblCfg viewState.neighborsIncoming (prevMsg Incoming) (nextMsg Incoming)
                ]

        ioIndicatorState =
            Just (inOutIndicator Nothing data.inDegree data.outDegree)
    in
    collapsibleSection vc "Neighbors" viewState.neighborsTableOpen ioIndicatorState content (AddressDetailsMsg AddressDetails.UserClickedToggleNeighborsTable)


longIdentDetailsHeadingView : View.Config -> Pathfinder.Config -> Id -> String -> List BtnConfig -> Html Msg
longIdentDetailsHeadingView vc _ id typeName annotations =
    let
        mNetwork =
            Id.network id

        heading =
            String.trim (String.join " " [ mNetwork, Locale.string vc.locale typeName ])
    in
    div []
        [ h1 [ panelHeadingStyle2 vc |> toAttr ] (Html.text (String.toUpper heading) :: (annotations |> List.map (annotationButton vc)))
        , copyableLongIdentifierPathfinder vc [ copyableIdentifierStyle vc |> toAttr ] (Id.id id)
        ]


annotationButton : View.Config -> BtnConfig -> Html Msg
annotationButton vc btn =
    disableableButton (linkButtonStyle vc) btn [ HA.title btn.text ] [ FontAwesome.icon btn.icon |> Html.fromUnstyled ]


apiAddressToRows : Api.Data.Address -> List KVTableRow
apiAddressToRows address =
    [ Row "Total received" (Currency address.totalReceived address.currency)
    , Row "Total sent" (Currency address.totalSpent address.currency)
    , Row "Balance" (Currency address.balance address.currency)
    , Gap
    , Row "First usage" (Timestamp address.firstTx.timestamp)
    , Row "Last usage" (Timestamp address.lastTx.timestamp)
    , Row "Cluster" (ValueHex address.entity)
    ]


apiUtxoTxToRows : Api.Data.TxUtxo -> List KVTableRow
apiUtxoTxToRows tx =
    [ Row "Timstamp" (TimestampWithTime tx.timestamp)
    , Gap
    , Row "Total Input" (Currency tx.totalInput tx.currency)
    , Row "Total Output" (Currency tx.totalOutput tx.currency)
    ]


detailsFactTableView : View.Config -> List KVTableRow -> Html Msg
detailsFactTableView vc rows =
    div [ smPaddingBottom |> toAttr ] [ renderKVTable vc rows ]


primaryButton : View.Config -> BtnConfig -> Html Msg
primaryButton vc btn =
    optionalTextButton vc Primary btn


secondaryButton : View.Config -> BtnConfig -> Html Msg
secondaryButton vc btn =
    optionalTextButton vc Secondary btn


optionalTextButton : View.Config -> ButtonType -> BtnConfig -> Html Msg
optionalTextButton vc bt btn =
    let
        ( iconattr, content ) =
            if String.isEmpty btn.text then
                ( [], [] )

            else
                ( [ smPaddingRight |> toAttr ], [ Html.text (Locale.string vc.locale btn.text) ] )
    in
    disableableButton (detailsActionButtonStyle vc bt)
        btn
        []
        (span iconattr [ FontAwesome.icon btn.icon |> Html.fromUnstyled ]
            :: content
        )


detailsActionButton : View.Config -> ButtonType -> BtnConfig -> Html Msg
detailsActionButton vc btnT btn =
    disableableButton (detailsActionButtonStyle vc btnT) btn [] [ Html.text (Locale.string vc.locale btn.text) ]


detailsActionsView : View.Config -> List BtnConfig -> Html Msg
detailsActionsView vc actionButtons =
    let
        btnType i =
            if i == 0 then
                Primary

            else
                Secondary
    in
    div [ smPaddingBottom |> toAttr ] (actionButtons |> List.indexedMap (\i itm -> detailsActionButton vc (btnType i) itm))


graphSvg : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> BBox -> Svg Msg
graphSvg plugins _ vc gc model bbox =
    let
        dim =
            { width = bbox.width, height = bbox.height }

        pointer =
            case ( model.dragging, model.pointerTool ) of
                ( Dragging _ _ _, Drag ) ->
                    Css.grabbing

                ( _, Select ) ->
                    Css.crosshair

                _ ->
                    Css.grab

        pointerStyle =
            [ Css.cursor pointer ]

        gradient name from to =
            linearGradient
                [ SA.id name
                ]
                [ stop
                    [ from
                        |> SA.stopColor
                    ]
                    []
                , stop
                    [ SA.offset "70%"
                    , to
                        |> SA.stopColor
                    ]
                    []
                ]

        originShiftX =
            searchBoxMinWidth / 2
    in
    svg
        ([ preserveAspectRatio "xMidYMid meet"
         , model.transform
            |> Transform.update { x = 0, y = 0 } { x = -originShiftX, y = 0 }
            |> Transform.viewBox dim
            |> viewBox
         , (Css.Graph.svgRoot vc ++ pointerStyle) |> SA.css
         , UserClickedGraph model.dragging
            |> onClick
         , SA.id "graph"
         , Svg.custom "wheel"
            (Json.Decode.map3
                (\y mx my ->
                    { message = UserWheeledOnGraph (mx + originShiftX) my y
                    , stopPropagation = False
                    , preventDefault = True
                    }
                )
                (Json.Decode.field "deltaY" Json.Decode.float)
                (Json.Decode.field "offsetX" Json.Decode.float)
                (Json.Decode.field "offsetY" Json.Decode.float)
            )
         , Svg.on "mousedown"
            (Util.Graph.decodeCoords Coords
                |> Json.Decode.map UserPushesLeftMouseButtonOnGraph
            )
         ]
            ++ (if model.dragging /= NoDragging then
                    Svg.preventDefaultOn "mousemove"
                        (Util.Graph.decodeCoords Coords
                            |> Json.Decode.map (\c -> ( UserMovesMouseOnGraph c, True ))
                        )
                        |> List.singleton

                else
                    []
               )
        )
        [ defs
            []
            [ gradient "utxoOutEdgeForth" vc.theme.pathfinder.edgeColor vc.theme.pathfinder.outEdgeColor
            , gradient "utxoInEdgeForth" vc.theme.pathfinder.inEdgeColor vc.theme.pathfinder.edgeColor
            , gradient "utxoOutEdgeBack" vc.theme.pathfinder.outEdgeColor vc.theme.pathfinder.edgeColor
            , gradient "utxoInEdgeBack" vc.theme.pathfinder.edgeColor vc.theme.pathfinder.inEdgeColor
            , gradient "accountOutEdgeForth" vc.theme.pathfinder.inEdgeColor vc.theme.pathfinder.outEdgeColor
            , gradient "accountInEdgeForth" vc.theme.pathfinder.outEdgeColor vc.theme.pathfinder.inEdgeColor
            , gradient "accountOutEdgeBack" vc.theme.pathfinder.outEdgeColor vc.theme.pathfinder.inEdgeColor
            , gradient "accountInEdgeBack" vc.theme.pathfinder.outEdgeColor vc.theme.pathfinder.inEdgeColor
            ]
        , Svg.lazy4 Network.addresses plugins vc gc model.network.addresses
        , Svg.lazy4 Network.txs plugins vc gc model.network.txs
        , Svg.lazy4 Network.edges plugins vc gc model.network.txs
        , drawDragSelector vc model

        -- , rect [ fill "red", width "3", height "3", x "8", y "0" ] [] -- Mark zero point in coordinate system
        ]


drawDragSelector : View.Config -> Model -> Svg Msg
drawDragSelector vc m =
    case ( m.dragging, m.pointerTool ) of
        ( Dragging tm start now, Select ) ->
            let
                originShiftX =
                    searchBoxMinWidth / 2

                crd =
                    case tm.state of
                        Settled c ->
                            c

                        Transitioning v ->
                            v.from

                z =
                    value crd.z

                xn =
                    (Basics.min start.x now.x + originShiftX) * z + crd.x

                yn =
                    Basics.min start.y now.y * z + crd.y

                widthn =
                    abs (start.x - now.x) * z

                heightn =
                    abs (start.y - now.y) * z

                pos =
                    Util.Graph.translate xn yn |> transform
            in
            rect [ Css.graphSelectionStyle vc |> css, pos, width (String.fromFloat widthn), height (String.fromFloat heightn), opacity "0.3" ]
                []

        _ ->
            rect [ x "0", y "0", width "0", height "0" ] []


dateRangePickerView : View.Config -> DateRangePicker.Model AddressDetails.Msg -> Html Msg
dateRangePickerView vc model =
    let
        startP =
            model.fromDate

        endP =
            model.toDate

        selectedDuration =
            Locale.durationPosix vc.locale 1 startP endP

        startS =
            Locale.posixDate vc.locale startP

        endS =
            Locale.posixDate vc.locale endP
    in
    div [ dateTimeRangeBoxStyle vc |> toAttr ]
        [ FontAwesome.iconWithOptions FontAwesome.calendar FontAwesome.Regular [] [] |> Html.fromUnstyled
        , span [] [ Html.text selectedDuration ]
        , span [ dateTimeRangeHighlightedDateStyle vc |> toAttr ] [ Html.text startS ]
        , span [] [ Html.text (Locale.string vc.locale "to") ]
        , span [ dateTimeRangeHighlightedDateStyle vc |> toAttr ] [ Html.text endS ]
        ]


transactionTableView : View.Config -> String -> (Id -> Bool) -> TransactionTable.Model -> Html Msg
transactionTableView vc currency txOnGraphFn model =
    let
        attributes =
            []

        prevMsg =
            \_ -> AddressDetailsMsg AddressDetails.UserClickedPreviousPageTransactionTable

        nextMsg =
            \_ -> AddressDetailsMsg AddressDetails.UserClickedNextPageTransactionTable

        styles =
            Css.Table.styles

        table =
            Table.pagedTableView vc
                attributes
                (TransactionTable.config styles vc currency txOnGraphFn)
                model.table
                prevMsg
                nextMsg

        filterRow drp =
            div
                [ css
                    [ Css.displayFlex
                    , Css.justifyContent Css.spaceBetween
                    , Css.marginBottom (Css.px 5)
                    ]
                ]
                [ drp
                , secondaryButton vc (BtnConfig FontAwesome.filter "" (AddressDetailsMsg <| AddressDetails.OpenDateRangePicker) True)
                ]
    in
    (case model.dateRangePicker of
        Just drp ->
            if DatePicker.isOpen drp.dateRangePicker then
                [ div []
                    [ primaryButton vc (BtnConfig FontAwesome.check "Ok" (AddressDetailsMsg <| AddressDetails.CloseDateRangePicker) True)
                    , secondaryButton vc (BtnConfig FontAwesome.times "Reset Filter" (AddressDetailsMsg <| AddressDetails.ResetDateRangePicker) True)
                    ]
                , DatePicker.view drp.settings drp.dateRangePicker
                    |> Html.fromUnstyled
                    |> Html.map AddressDetailsMsg
                ]

            else
                [ dateRangePickerView vc drp
                    |> filterRow
                , table
                ]

        Nothing ->
            [ filterRow none
            , table
            ]
    )
        |> div []
