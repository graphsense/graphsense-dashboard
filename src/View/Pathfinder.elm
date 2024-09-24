module View.Pathfinder exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Color exposing (Color)
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Graph
import Css.Pathfinder as Css exposing (fullWidth)
import Css.Table
import Css.View
import Dict
import DurationDatePicker as DatePicker
import FontAwesome
import Hex
import Hovercard
import Html.Styled as Html exposing (Html, button, div, h2, img, span, table, td, tr)
import Html.Styled.Attributes as HA exposing (src)
import Html.Styled.Events exposing (onMouseEnter, onMouseLeave)
import Init.Pathfinder.Id as Id
import Json.Decode
import Model.Currency as Asset exposing (Currency(..), asset, assetFromBase)
import Model.DateRangePicker as DateRangePicker
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (BBox, Coords)
import Model.Graph.Table
import Model.Graph.Transform exposing (Transition(..))
import Model.Locale as Locale
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network exposing (Network)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Model.Pathfinder.Tx as Tx
import Model.Pathfinder.TxDetails as TxDetails
import Model.Tx as Tx
import Msg.Pathfinder exposing (DisplaySettingsMsg(..), IoDirection(..), Msg(..), TxDetailsMsg(..))
import Msg.Pathfinder.AddressDetails as AddressDetails
import Number.Bounded exposing (value)
import Plugin.Model exposing (ModelState)
import Plugin.View exposing (Plugins)
import RecordSetter as Rs
import RemoteData
import Route
import Route.Graph exposing (AddressTable(..))
import Svg.Styled exposing (Svg, defs, linearGradient, stop, svg)
import Svg.Styled.Attributes exposing (css, height, id, offset, preserveAspectRatio, stopColor, transform, viewBox, width)
import Svg.Styled.Events as Svg
import Svg.Styled.Lazy as Svg
import Theme.Colors as Colors
import Theme.Html.Icons as HIcons
import Theme.Html.SettingsComponents as SettingsComponents
import Theme.Html.SidePanelComponents as SidePanelComponents
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.Icons as Icons
import Update.Graph.Transform as Transform
import Util.Css as Css
import Util.Data as Data
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.Graph
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags)
import Util.View exposing (copyIconPathfinder, hovercard, none, truncateLongIdentifierWithLengths)
import View.Graph.Table exposing (noTools)
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.Address as Address
import View.Pathfinder.Icons exposing (inIcon, outIcon)
import View.Pathfinder.Network as Network
import View.Pathfinder.PagedTable as PagedTable
import View.Pathfinder.Table.IoTable as IoTable
import View.Pathfinder.Table.TransactionTable as TransactionTable
import View.Pathfinder.Toolbar as Toolbar
import View.Pathfinder.Tooltip as Tooltip
import View.Pathfinder.Utils exposing (dateFromTimestamp, multiLineDateTimeFromTimestamp)
import View.Search


type alias BtnConfig =
    { icon : Bool -> Html Msg, text : String, msg : Msg, enable : Bool }


inlineCloseSmallIcon : Html Msg
inlineCloseSmallIcon =
    HIcons.iconsCloseSmallWithAttributes HIcons.iconsCloseSmallAttributes {}


inlineDoneSmallIcon : Html Msg
inlineDoneSmallIcon =
    HIcons.iconsDoneSmallWithAttributes HIcons.iconsDoneSmallAttributes {}


inlineClusterIcon : Bool -> Color -> Html Msg
inlineClusterIcon highlight clr =
    let
        getHighlight c =
            if highlight then
                [ css ((Util.View.toCssColor >> Css.fill >> Css.important >> List.singleton) c) ]

            else
                []
    in
    HIcons.iconsUntaggedWithAttributes
        (HIcons.iconsUntaggedAttributes
            |> Rs.s_ellipse25 (getHighlight clr)
        )
        {}



-- Helpers


graphActionButtons : String -> List BtnConfig
graphActionButtons name =
    [-- BtnConfig (\_ -> inlineExportIcon) "Export" (UserClickedExportGraphAsPNG name) True
    ]


type ValueType
    = ValueInt Int
    | ValueHex Int
    | Boolean Bool
    | Text String
    | Currency Api.Data.Values Asset.AssetIdentifier
    | CurrencyWithCode Api.Data.Values Asset.AssetIdentifier
    | InOut (Maybe Int) Int Int
    | Timestamp Int
    | TimestampWithTime Int
    | CopyIdent String


type KVTableRow
    = Row String ValueType
    | LinkRow String String
    | Gap


renderKVTable : View.Config -> List KVTableRow -> Html Msg
renderKVTable vc rows =
    table [ fullWidth |> css ] (rows |> List.map (renderKVRow vc))


renderKVRow : View.Config -> KVTableRow -> Html Msg
renderKVRow vc row =
    case row of
        Row key value ->
            tr []
                [ td [ Css.kVTableKeyTdStyle vc |> css ] [ Html.text (Locale.string vc.locale key) ]
                , td [ Css.kVTableValueTdStyle vc |> css ] (renderValueTypeValue vc value |> List.singleton)
                , td [ Css.kVTableTdStyle vc |> css ] (renderValueTypeExtension vc value |> List.singleton)
                ]

        Gap ->
            tr []
                [ td [ Css.kVTableKeyTdStyle vc |> css ] []
                , td [] []
                , td [] []
                ]

        LinkRow n l ->
            tr []
                [ td [ Css.kVTableKeyTdStyle vc |> css ] []
                , td [] []
                , td [] [ Html.a [ HA.href l, Css.View.link vc |> css ] [ Locale.text vc.locale n ] ]
                ]


renderValueTypeValue : View.Config -> ValueType -> Html Msg
renderValueTypeValue vc val =
    case val of
        ValueInt v ->
            span [] [ Html.text (Locale.int vc.locale v) ]

        ValueHex v ->
            span [] [ Html.text (Hex.toString v) ]

        InOut total inv outv ->
            inOutIndicatorOld vc total inv outv

        Text txt ->
            span [] [ Html.text txt ]

        Currency v asset ->
            span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.currencyWithoutCode2 vc.locale [ ( asset, v ) ]) ]

        CurrencyWithCode v asset ->
            -- span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.coinWithoutCode vc.locale (assetFromBase ticker) v.value ++ " " ++ ticker) ]
            span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.currencyWithoutCode vc.locale [ ( asset, v ) ]) ]

        CopyIdent ident ->
            Util.View.copyableLongIdentifierPathfinder vc [] ident

        Timestamp ts ->
            span [] [ Locale.timestampDateUniform vc.locale ts |> Html.text ]

        TimestampWithTime ts ->
            span [] [ multiLineDateTimeFromTimestamp vc ts ]

        Boolean tv ->
            span []
                [ Html.text
                    (Locale.string vc.locale
                        (if tv then
                            "yes"

                         else
                            "no"
                        )
                    )
                ]


renderValueTypeExtension : View.Config -> ValueType -> Html Msg
renderValueTypeExtension vc val =
    case val of
        Currency _ asset ->
            span []
                [ Html.text
                    (String.toUpper
                        (case vc.locale.currency of
                            Coin ->
                                asset.asset

                            Fiat x ->
                                x
                        )
                    )
                ]

        _ ->
            none


disableableButton : (Bool -> List Css.Style) -> BtnConfig -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
disableableButton style btn attrs content =
    let
        addattr =
            if btn.enable then
                [ btn.msg |> Svg.onClick ]

            else
                [ HA.disabled True ]
    in
    button (((style btn.enable |> css) :: addattr) ++ attrs) content


inOutIndicator : View.Config -> String -> Int -> Int -> Int -> Html Msg
inOutIndicator vc title mnr inNr outNr =
    SidePanelComponents.sidePanelListHeaderTitleTransactions
        { sidePanelListHeaderTitleTransactions =
            { totalNumber = Locale.int vc.locale mnr
            , incomingNumber = Locale.int vc.locale inNr
            , outgoingNumber = Locale.int vc.locale outNr
            , title = title
            }
        }


inOutIndicatorOld : View.Config -> Maybe Int -> Int -> Int -> Html Msg
inOutIndicatorOld vc mnr inNr outNr =
    let
        prefix =
            String.trim (String.join " " [ mnr |> Maybe.map (Locale.int vc.locale >> (++) " - ") |> Maybe.withDefault "", "(" ])
    in
    span [ Css.ioOutIndicatorStyle |> css ] [ Html.text prefix, inIcon, Html.text (Locale.int vc.locale inNr), outIcon, Html.text (Locale.int vc.locale outNr), Html.text ")" ]



-- View


view : Plugins -> ModelState -> View.Config -> Pathfinder.Model -> { navbar : List (Html Msg), contents : List (Html Msg) }
view plugins states vc model =
    { navbar = []
    , contents = graph plugins states vc model.config model
    }


graph : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> List (Html Msg)
graph plugins states vc gc model =
    [ vc.size
        |> Maybe.map (graphSvg plugins states vc gc model)
        |> Maybe.withDefault none
    , topLeftPanel vc
    , topCenterPanel plugins states vc gc model
    , topRightPanel plugins states vc gc model
    ]
        ++ (model.tooltip
                |> Maybe.map (Tooltip.view vc model.tagSummaries)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
           )
        ++ (model.config.displaySettingsHovercard
                |> Maybe.map (settingsView vc model)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
           )


topCenterPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> Html Msg
topCenterPanel plugins ms vc gc model =
    div
        [ css Css.topPanelStyle
        ]
        [ h2 [ vc.theme.heading2 |> css ] [ Html.text "Pathfinder" ]
        , div
            [ css
                [ Css.displayFlex
                , Css.property "gap" "10px"
                , Css.property "pointer-events" "all"
                ]
            ]
            [ searchBoxView plugins ms vc gc model
            , Toolbar.view vc
                { undoDisabled = List.isEmpty model.history.past
                , redoDisabled = List.isEmpty model.history.future
                , deleteDisabled = model.selection == Pathfinder.NoSelection
                , pointerTool = model.pointerTool
                , exportName = model.name
                }
            ]
        , div
            [ css [ Css.property "pointer-events" "all" ] ]
            [ graphActionsView vc gc model
            ]
        ]


topLeftPanel : View.Config -> Html Msg
topLeftPanel vc =
    div [ Css.topLeftPanelStyle vc |> css ]
        []


settingsView : View.Config -> Pathfinder.Model -> Hovercard.Model -> Html Msg
settingsView vc _ hc =
    let
        utc_text =
            if vc.showDatesInUserLocale then
                "User"

            else
                "UTC"
    in
    div [ css [ Css.padding Css.mlGap ] ]
        [ div [ Css.panelHeadingStyle3 vc |> css ] [ Html.text (Locale.string vc.locale "Transaction") ]
        , Util.View.onOffSwitch vc [ HA.checked vc.showTimestampOnTxEdge, Svg.onClick (UserClickedToggleShowTxTimestamp |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "Show timestamp")
        , div [ Css.panelHeadingStyle3 vc |> css ] [ Html.text (Locale.string vc.locale "Date") ]
        , Util.View.onOffSwitch vc [ HA.checked vc.showDatesInUserLocale, Svg.onClick (UserClickedToggleDatesInUserLocale |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale utc_text)
        , Util.View.onOffSwitch vc [ HA.checked vc.showTimeZoneOffset, Svg.onClick (UserClickedToggleShowTimeZoneOffset |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "Show timezone")
        , div [ Css.panelHeadingStyle3 vc |> css ] [ Html.text (Locale.string vc.locale "Cluster") ]
        , Util.View.onOffSwitch vc [ HA.checked vc.highlightClusterFriends, Svg.onClick (UserClickedToggleHighlightClusterFriends |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "Highlight clusters")
        ]
        |> Html.toUnstyled
        |> List.singleton
        |> hovercard vc hc (Css.zIndexMainValue + 1)


topRightPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> Html Msg
topRightPanel _ _ vc gc model =
    div [ Css.topRightPanelStyle vc |> css ]
        [ detailsView vc gc model
        ]


graphActionsView : View.Config -> Pathfinder.Config -> Pathfinder.Model -> Html Msg
graphActionsView vc _ m =
    div [ Css.graphActionsViewStyle vc |> css ]
        (graphActionButtons m.name |> List.map (graphActionButton vc))


graphActionButton : View.Config -> BtnConfig -> Html Msg
graphActionButton vc btn =
    disableableButton (Css.graphActionButtonStyle vc) btn [] (iconWithText vc (btn.icon True) (Locale.string vc.locale btn.text))


iconWithText : View.Config -> Html Msg -> String -> List (Html Msg)
iconWithText _ faIcon text =
    let
        s =
            if String.length text > 0 then
                Css.iconWithTextStyle

            else
                []
    in
    [ span [ s |> css ] [ faIcon ], Html.text text ]


searchBoxView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> Html Msg
searchBoxView plugins _ vc _ model =
    SettingsComponents.toolbarSearchFieldWithInstances
        (SettingsComponents.toolbarSearchFieldAttributes
            |> Rs.s_toolbarSearchField
                [ css [ Css.alignItems Css.stretch |> Css.important ] ]
        )
        (SettingsComponents.toolbarSearchFieldInstances
            |> Rs.s_searchInputField
                (View.Search.searchWithMoreCss plugins
                    vc
                    { css = Css.searchInputStyle vc
                    , formCss =
                        Just
                            [ Css.flexGrow <| Css.num 1
                            , Css.height Css.auto |> Css.important
                            ]
                    , frameCss =
                        Just
                            [ Css.height <| Css.pct 100
                            , Css.marginRight Css.zero |> Css.important
                            ]
                    , multiline = False
                    , resultsAsLink = True
                    , showIcon = False
                    }
                    model.search
                    |> Html.map SearchMsg
                    |> Just
                )
        )
        {}


detailsView : View.Config -> Pathfinder.Config -> Pathfinder.Model -> Html Msg
detailsView vc gc model =
    case model.details of
        Just details ->
            case details of
                Pathfinder.AddressDetails id state ->
                    RemoteData.unwrap
                        (Util.View.loadingSpinner vc Css.View.loadingSpinner)
                        (addressDetailsContentView vc gc model id)
                        state

                Pathfinder.TxDetails id state ->
                    txDetailsContentView vc gc model id state

        Nothing ->
            none


txDetailsContentView : View.Config -> Pathfinder.Config -> Pathfinder.Model -> Id -> TxDetails.Model -> Html Msg
txDetailsContentView vc _ model id viewState =
    let
        getLbl id_ =
            Dict.get id_ model.tagSummaries
                |> Maybe.withDefault Pathfinder.NoTags
    in
    case viewState.tx.type_ of
        Tx.Utxo tx ->
            let
                style =
                    [ css [ Css.width (Css.pct 100) ] ]
            in
            SidePanelComponents.sidePanelTransactionWithAttributes
                (SidePanelComponents.sidePanelTransactionAttributes
                    |> Rs.s_sidePanelTransaction
                        [ sidePanelCss
                            |> css
                        ]
                )
                { identifierWithCopyIcon =
                    { identifier = Id.id id |> truncateLongIdentifierWithLengths 8 4
                    , copyIconInstance = Id.id id |> copyIconPathfinder vc
                    }
                , leftTab = { variant = none }
                , rightTab = { variant = none }
                , titleOfTimestamp = { infoLabel = Locale.string vc.locale "Timestamp" }
                , valueOfTimestamp = timeToCell vc tx.raw.timestamp
                , titleOfTxValue = { infoLabel = Locale.string vc.locale "Value" }
                , valueOfTxValue = valuesToCell vc (assetFromBase tx.raw.currency) tx.raw.totalOutput
                , sidePanelTransaction =
                    { tabsVisible = False
                    , inputListInstance =
                        let
                            headerTitle =
                                { sidePanelListHeaderTitleInputs =
                                    { title = Locale.string vc.locale "Inputs"
                                    , totalNumber = Locale.int vc.locale tx.raw.noInputs
                                    }
                                }

                            headerEvent =
                                [ Svg.onClick (TxDetailsMsg (UserClickedToggleIoTable Inputs))
                                , css [ Css.cursor Css.pointer ]
                                ]
                        in
                        if viewState.inputsTableOpen then
                            SidePanelComponents.sidePanelInputListOpenWithAttributes
                                (SidePanelComponents.sidePanelInputListOpenAttributes
                                    |> Rs.s_sidePanelInputListOpen style
                                    |> Rs.s_sidePanelInputListHeaderOpen headerEvent
                                )
                                { sidePanelInputListOpen =
                                    { listInstance = ioTableView vc Inputs model.network tx.raw.currency viewState.inputsTable getLbl
                                    }
                                , sidePanelInputListHeaderOpen =
                                    { titleInstance = SidePanelComponents.sidePanelListHeaderTitleInputs headerTitle }
                                }

                        else
                            SidePanelComponents.sidePanelInputListHeaderClosedWithAttributes
                                (SidePanelComponents.sidePanelInputListHeaderClosedAttributes
                                    |> Rs.s_sidePanelInputListHeaderClosed (headerEvent ++ style)
                                )
                                headerTitle
                    , outputListInstance =
                        let
                            headerTitle =
                                { title = Locale.string vc.locale "Outputs"
                                , totalNumber = Locale.int vc.locale tx.raw.noOutputs
                                }

                            headerEvent =
                                [ Svg.onClick (TxDetailsMsg (UserClickedToggleIoTable Outputs))
                                , css [ Css.cursor Css.pointer ]
                                ]
                        in
                        if viewState.outputsTableOpen then
                            SidePanelComponents.sidePanelOutputListOpenWithAttributes
                                (SidePanelComponents.sidePanelOutputListOpenAttributes |> Rs.s_sidePanelOutputListOpen style |> Rs.s_sidePanelOutputListHeaderOpen headerEvent)
                                { sidePanelOutputListOpen = { listInstance = ioTableView vc Outputs model.network tx.raw.currency viewState.outputsTable getLbl }
                                , sidePanelListHeaderTitleOutputs = headerTitle
                                }

                        else
                            SidePanelComponents.sidePanelOutputListHeaderClosedWithAttributes
                                (SidePanelComponents.sidePanelOutputListHeaderClosedAttributes |> Rs.s_sidePanelOutputListHeaderClosed (headerEvent ++ style))
                                { sidePanelListHeaderTitleOutputs = headerTitle }
                    }
                , sidePanelTxHeader =
                    { headerText =
                        (String.toUpper <| Id.network id) ++ " " ++ Locale.string vc.locale "Transaction"
                    }
                }

        Tx.Account tx ->
            SidePanelComponents.sidePanelEthTransactionWithAttributes
                (SidePanelComponents.sidePanelEthTransactionAttributes
                    |> Rs.s_sidePanelEthTransaction
                        [ sidePanelCss
                            |> css
                        ]
                )
                { identifierWithCopyIcon =
                    { identifier = Id.id id |> truncateLongIdentifierWithLengths 8 4
                    , copyIconInstance = Id.id id |> copyIconPathfinder vc
                    }
                , leftTab = { variant = none }
                , rightTab = { variant = none }
                , titleOfTimestamp = { infoLabel = Locale.string vc.locale "Timestamp" }
                , valueOfTimestamp = timeToCell vc tx.raw.timestamp
                , titleOfEstimatedValue = { infoLabel = Locale.string vc.locale "Value" }
                , valueOfEstimatedValue = valuesToCell vc (asset tx.raw.network tx.raw.currency) tx.value
                , titleOfSender = { infoLabel = Locale.string vc.locale "Sender" }
                , valueOfSender =
                    { firstRowText = Id.id tx.from |> truncateLongIdentifierWithLengths 8 4
                    , copyIconInstance = Id.id tx.from |> copyIconPathfinder vc
                    }
                , titleOfReceiver = { infoLabel = Locale.string vc.locale "Receiver" }
                , valueOfReceiver =
                    { firstRowText = Id.id tx.to |> truncateLongIdentifierWithLengths 8 4
                    , copyIconInstance = Id.id tx.to |> copyIconPathfinder vc
                    }
                , sidePanelEthTransaction =
                    { tabsVisible = False
                    }
                , sidePanelEthTxDetails =
                    { contractCreationVisible = tx.raw.contractCreation |> Maybe.withDefault False
                    }
                , sidePanelTxHeader =
                    { headerText =
                        tx.raw.identifier
                            |> Tx.parseTxIdentifier
                            |> Maybe.map Tx.txTypeToLabel
                            |> Maybe.withDefault "Transaction"
                            |> Locale.string vc.locale
                            |> (++) ((String.toUpper <| Id.network id) ++ " ")
                    }
                , titleOfContractCreation = { infoLabel = Locale.string vc.locale "contract creation" }
                , valueOfContractCreation =
                    { firstRowText =
                        Locale.string vc.locale <|
                            if tx.raw.contractCreation |> Maybe.withDefault False then
                                "yes"

                            else
                                "no"
                    , secondRowText = ""
                    , secondRowVisible = False
                    }
                }


ioTableView : View.Config -> IoDirection -> Network -> String -> Model.Graph.Table.Table Api.Data.TxValue -> (Id -> Pathfinder.HavingTags) -> Html Msg
ioTableView vc dir network currency table getLbl =
    let
        isCheckedFn =
            flip Network.hasAddress network

        styles =
            Css.Table.styles
                |> Rs.s_root (\vc_ -> Css.Table.styles.root vc_ ++ [ Css.display Css.block, Css.width (Css.pct 100), Css.paddingTop Css.lGap ])
    in
    View.Graph.Table.table
        styles
        vc
        [ css [ Css.overflowY Css.auto, Css.maxHeight (Css.px ((vc.size |> Maybe.map .height |> Maybe.withDefault 500) * 0.5)) ] ]
        noTools
        (IoTable.config styles vc dir currency isCheckedFn (Just getLbl))
        table


valuesToCell : View.Config -> Asset.AssetIdentifier -> Api.Data.Values -> { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
valuesToCell vc asset value =
    { firstRowText = Locale.currency vc.locale [ ( asset, value ) ]
    , secondRowText = ""
    , secondRowVisible = False
    }


timeToCell : View.Config -> Int -> { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
timeToCell vc d =
    { firstRowText = Locale.timestampDateUniform vc.locale d
    , secondRowText = Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset d
    , secondRowVisible = True
    }


sidePanelCss : List Css.Style
sidePanelCss =
    [ Css.calc (Css.vh 100) Css.minus (Css.px 150) |> Css.maxHeight
    , Css.overflowY Css.auto
    , Css.overflowX Css.hidden
    , Css.paddingTop (Css.px 10)
    ]


addressDetailsContentView : View.Config -> Pathfinder.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Html Msg
addressDetailsContentView vc gc model id viewState =
    let
        address =
            model.network.addresses
                |> Dict.get id

        ts =
            case Dict.get id model.tagSummaries of
                Just (Pathfinder.HasTagSummary t) ->
                    Just t

                _ ->
                    Nothing

        nrTagsAddress =
            ts |> Maybe.map .tagCount |> Maybe.withDefault 0

        tagLabels =
            ts
                |> Maybe.map
                    (\x ->
                        if hasOnlyExchangeTags x then
                            []

                        else
                            (.labelSummary >> Dict.toList >> List.sortBy (Tuple.second >> .relevance) >> List.reverse) x
                    )
                |> Maybe.withDefault []

        lenTagLabels =
            List.length tagLabels

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

        clstrId =
            Id.initClusterId viewState.data.currency viewState.data.entity

        showExchangeTag =
            actorText /= Nothing

        showOtherTag =
            List.isEmpty tagLabels |> not

        showTag i ( tid, t ) =
            let
                link =
                    Route.Graph.addressRoute { currency = Id.network id, address = Id.id id, layer = Nothing, table = Just AddressTagsTable }
                        |> Route.Graph
                        |> Route.toUrl
            in
            Html.a
                [ onMouseEnter (UserMovesMouseOverTagLabel tid)
                , onMouseLeave (UserMovesMouseOutTagLabel tid)

                --, Css.tagLinkButtonStyle vc |> css
                , HA.css SidePanelComponents.sidePanelAddressLabelOfTags_details.styles
                , HA.id tid
                , HA.href link
                ]
                (Html.text t.label
                    :: (if i < (lenTagLabels - 1) then
                            [ Html.text "," ]

                        else
                            []
                       )
                )

        nMaxTags =
            3

        nTagsToShow =
            if gc.displayAllTagsInDetails then
                lenTagLabels

            else
                nMaxTags

        tagsControl =
            if lenTagLabels > nMaxTags then
                if gc.displayAllTagsInDetails then
                    Html.span [ Css.tagLinkButtonStyle vc |> css, HA.title (Locale.string vc.locale "show less..."), Svg.onClick UserClickedToggleDisplayAllTagsInDetails ] [ Html.text (Locale.string vc.locale "less...") ]

                else
                    Html.span [ Css.tagLinkButtonStyle vc |> css, HA.title (Locale.string vc.locale "show more..."), Svg.onClick UserClickedToggleDisplayAllTagsInDetails ] [ Html.text ("+" ++ String.fromInt (lenTagLabels - nMaxTags) ++ " "), Html.text (Locale.string vc.locale "more...") ]

            else
                none

        -- clusterHighlightAttr =
        --     if vc.highlightClusterFriends then
        --         Colors.getAssignedColor Colors.Clusters clstrid model.colors
        --             |> Maybe.map
        --                 (.color
        --                     >> Util.View.toCssColor
        --                     >> Css.fill
        --                     >> Css.important
        --                     >> List.singleton
        --                     >> css
        --                     >> List.singleton
        --                 )
        --             |> Maybe.withDefault []
        --     else
        --         []
        assetId =
            assetFromBase viewState.data.currency

        sidePanelData =
            { actorIconInstance =
                actorImg
                    |> Maybe.map
                        (\imgSrc ->
                            let
                                iconDetails =
                                    HIcons.iconsAssign_details
                            in
                            img
                                [ src imgSrc
                                , HA.alt <| Maybe.withDefault "" <| actorText
                                , HA.width <| round iconDetails.width
                                , HA.height <| round iconDetails.height
                                , HA.css iconDetails.styles
                                ]
                                []
                                |> List.singleton
                                |> div
                                    [ HA.css iconDetails.styles
                                    , HA.css
                                        [ iconDetails.width
                                            |> Css.px
                                            |> Css.width
                                        , iconDetails.height
                                            |> Css.px
                                            |> Css.height
                                        ]
                                    ]
                        )
                    |> Maybe.withDefault (Icons.iconsAssignSvg [] {})
            , tabsVisible = False
            , actorAndTagVisible = showExchangeTag || showOtherTag
            , listInstance =
                let
                    titleInstance =
                        { titleInstance = inOutIndicator vc "Transactions" (viewState.data.noIncomingTxs + viewState.data.noOutgoingTxs) viewState.data.noIncomingTxs viewState.data.noOutgoingTxs
                        }

                    style =
                        [ css [ Css.width (Css.pct 100) ] ]

                    headerEvent =
                        [ Svg.onClick (AddressDetailsMsg AddressDetails.UserClickedToggleTransactionTable)
                        , css [ Css.cursor Css.pointer ]
                        ]
                in
                if viewState.transactionsTableOpen then
                    SidePanelComponents.sidePanelTxListOpenWithAttributes
                        (SidePanelComponents.sidePanelTxListOpenAttributes
                            |> Rs.s_sidePanelTxListOpen style
                            |> Rs.s_sidePanelTxListHeaderOpen headerEvent
                        )
                        { sidePanelTxListHeaderOpen = titleInstance
                        , sidePanelTxListOpen =
                            { listInstance =
                                transactionTableView vc id txOnGraphFn viewState.txs
                            }
                        }

                else
                    SidePanelComponents.sidePanelTxListClosedWithAttributes
                        (SidePanelComponents.sidePanelTxListClosedAttributes
                            |> Rs.s_sidePanelTxListClosed (style ++ headerEvent)
                        )
                        { sidePanelTxListClosed = titleInstance
                        }
            , actorVisible = showExchangeTag
            , tagsVisible = showOtherTag
            }

        sidePanelAddressHeader =
            { iconInstance =
                Address.toNodeIconHtml False address (Dict.get clstrId model.clusters) (Colors.getAssignedColor Colors.Clusters clstrId model.colors |> Maybe.map .color)
            , headerText =
                (String.toUpper <| Id.network id)
                    ++ " "
                    ++ (if viewState.data.isContract |> Maybe.withDefault False then
                            Locale.string vc.locale "Smart Contract"

                        else
                            Locale.string vc.locale "address"
                       )
            }

        sidePanelAddressDetails =
            { clusterInfoVisible = Dict.member clstrId model.clusters
            , clusterInfoInstance =
                Dict.get clstrId model.clusters
                    |> Maybe.map (clusterInfoView vc model.config.isClusterDetailsOpen model.colors nrTagsAddress)
                    |> Maybe.withDefault none
            }

        sidePanelAddressCopyIcon =
            { identifier = Id.id id |> truncateLongIdentifierWithLengths 8 4
            , copyIconInstance = Id.id id |> copyIconPathfinder vc
            }

        labelOfTags =
            Just
                (div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.row
                        , Css.flexWrap Css.wrap
                        , Css.property "gap" "1ex"
                        , Css.alignItems Css.center
                        , Css.width <| Css.px (SidePanelComponents.sidePanelAddress_details.width * 0.8)
                        ]
                    ]
                    ((tagLabels |> List.take nTagsToShow |> List.indexedMap showTag) ++ [ tagsControl ])
                )

        labelOfActor =
            actor_id
                |> Maybe.map
                    (\aid ->
                        let
                            link =
                                Route.Graph.actorRoute aid Nothing
                                    |> Route.Graph
                                    |> Route.toUrl

                            text =
                                actorText |> Maybe.withDefault ""
                        in
                        Html.a
                            [ HA.href link
                            , css SidePanelComponents.sidePanelAddressLabelOfTags_details.styles
                            ]
                            [ Html.text text
                            ]
                    )

        fiatCurr =
            vc.preferredFiatCurrency

        toTokenRow i ( symbol, values ) =
            let
                ass =
                    asset viewState.data.currency symbol

                value =
                    Locale.coin vc.locale ass values.value

                fvalue =
                    Locale.getFiatValue fiatCurr values
            in
            if modBy 2 (i + 1) == 0 then
                SidePanelComponents.tokenRowStateNeutral
                    { stateNeutral =
                        { fiatValue = fvalue |> Maybe.map (Locale.fiat vc.locale fiatCurr) |> Maybe.withDefault ""
                        , tokenCode = String.toUpper symbol
                        , tokenName = String.toUpper symbol
                        , tokenValue = value
                        }
                    }

            else
                SidePanelComponents.tokenRowStateHighlight
                    { stateHighlight =
                        { fiatValue = fvalue |> Maybe.map (Locale.fiat vc.locale fiatCurr) |> Maybe.withDefault ""
                        , tokenCode = String.toUpper symbol
                        , tokenName = String.toUpper symbol
                        , tokenValue = value
                        }
                    }

        tokenRows =
            div
                [ SidePanelComponents.tokensDropDownOpenTokensListTokensList_details.styles
                    ++ [ Css.position Css.absolute
                       , Css.zIndex (Css.int (Css.zIndexMainValue + 1))
                       , Css.top (Css.px SidePanelComponents.tokensDropDownClosed_details.height)
                       , Css.width (Css.px SidePanelComponents.tokensDropDownOpen_details.width)
                       ]
                    |> css
                ]
                (viewState.data.tokenBalances |> Maybe.withDefault Dict.empty |> Dict.toList |> List.indexedMap toTokenRow)

        ntokensString =
            "(" ++ (viewState.data.tokenBalances |> Maybe.withDefault Dict.empty |> Dict.size |> String.fromInt) ++ " tokens)"

        fiatSum =
            viewState.data.tokenBalances |> Maybe.withDefault Dict.empty |> Dict.toList |> List.filterMap (Tuple.second >> Locale.getFiatValue fiatCurr) |> List.sum

        valueSumString =
            Locale.fiat vc.locale fiatCurr fiatSum

        attrClickSelect =
            [ Svg.onClick (AddressDetails.UserClickedToggleTokenBalancesSelect |> AddressDetailsMsg), [ Css.cursor Css.pointer ] |> css ]

        tokensDropDownOpen =
            SidePanelComponents.tokensDropDownOpenWithInstances
                (SidePanelComponents.tokensDropDownOpenAttributes
                    |> Rs.s_tokensDropDownHeaderOpen attrClickSelect
                )
                (SidePanelComponents.tokensDropDownOpenInstances
                    |> Rs.s_tokensList (Just tokenRows)
                )
                { tokenRow1 =
                    { variant = none
                    }
                , tokenRow2 =
                    { variant = none
                    }
                , tokenRow3 =
                    { variant = none
                    }
                , tokensDropDownHeaderOpen =
                    { numberOfToken = ntokensString, totalTokenValue = valueSumString }
                }

        tokensDropDownClosed =
            SidePanelComponents.tokensDropDownClosedWithInstances
                (SidePanelComponents.tokensDropDownClosedAttributes
                    |> Rs.s_tokensDropDownClosed attrClickSelect
                )
                SidePanelComponents.tokensDropDownClosedInstances
                { tokensDropDownClosed = { numberOfToken = ntokensString, totalTokenValue = valueSumString } }

        tokensDropdown =
            if viewState.tokenBalancesOpen then
                tokensDropDownOpen

            else
                tokensDropDownClosed
    in
    if Data.isAccountLike (Id.network id) then
        SidePanelComponents.sidePanelEthAddressWithInstances
            (SidePanelComponents.sidePanelEthAddressAttributes
                |> Rs.s_sidePanelEthAddress
                    [ sidePanelCss
                        |> css
                    ]
            )
            (SidePanelComponents.sidePanelEthAddressInstances
                |> Rs.s_labelOfTags
                    labelOfTags
                |> Rs.s_labelOfActor
                    labelOfActor
                |> Rs.s_tokensDropDownClosed (Just tokensDropdown)
            )
            { identifierWithCopyIcon = sidePanelAddressCopyIcon
            , leftTab = { variant = none }
            , rightTab = { variant = none }
            , sidePanelAddressHeader = sidePanelAddressHeader
            , sidePanelEthAddress = sidePanelData
            , sidePanelEthAddressDetails = sidePanelAddressDetails
            , sidePanelRowWithDropdown = { valueCellInstance = none }
            , tokensDropDownClosed = { numberOfToken = ntokensString, totalTokenValue = valueSumString }
            , titleOfEthBalance = { infoLabel = Locale.string vc.locale "Balance" ++ " " ++ String.toUpper viewState.data.currency }
            , titleOfSidePanelRowWithDropdown = { infoLabel = Locale.string vc.locale "Token holdings" }
            , valueOfEthBalance = valuesToCell vc assetId viewState.data.balance
            , titleOfTotalReceived = { infoLabel = Locale.string vc.locale "Total received" }
            , valueOfTotalReceived = valuesToCell vc assetId viewState.data.totalReceived
            , titleOfTotalSent = { infoLabel = Locale.string vc.locale "Total sent" }
            , valueOfTotalSent = valuesToCell vc assetId viewState.data.totalSpent
            , titleOfLastUsage = { infoLabel = Locale.string vc.locale "Last usage" }
            , valueOfLastUsage = timeToCell vc viewState.data.lastTx.timestamp
            , titleOfFirstUsage = { infoLabel = Locale.string vc.locale "First usage" }
            , valueOfFirstUsage = timeToCell vc viewState.data.firstTx.timestamp
            }

    else
        SidePanelComponents.sidePanelAddressWithInstances
            (SidePanelComponents.sidePanelAddressAttributes
                |> Rs.s_sidePanelAddress
                    [ sidePanelCss
                        |> css
                    ]
            )
            (SidePanelComponents.sidePanelAddressInstances
                |> Rs.s_labelOfTags
                    labelOfTags
                |> Rs.s_labelOfActor
                    labelOfActor
            )
            { sidePanelAddress = sidePanelData
            , leftTab = { variant = none }
            , rightTab = { variant = none }
            , identifierWithCopyIcon = sidePanelAddressCopyIcon
            , sidePanelAddressDetails = sidePanelAddressDetails
            , sidePanelAddressHeader = sidePanelAddressHeader
            , titleOfBalance = { infoLabel = Locale.string vc.locale "Balance" }
            , valueOfBalance = valuesToCell vc assetId viewState.data.balance
            , titleOfTotalReceived = { infoLabel = Locale.string vc.locale "Total received" }
            , valueOfTotalReceived = valuesToCell vc assetId viewState.data.totalReceived
            , titleOfTotalSent = { infoLabel = Locale.string vc.locale "Total sent" }
            , valueOfTotalSent = valuesToCell vc assetId viewState.data.totalSpent
            , titleOfLastUsage = { infoLabel = Locale.string vc.locale "Last usage" }
            , valueOfLastUsage = timeToCell vc viewState.data.lastTx.timestamp
            , titleOfFirstUsage = { infoLabel = Locale.string vc.locale "First usage" }
            , valueOfFirstUsage = timeToCell vc viewState.data.firstTx.timestamp
            }


apiEntityToRows : Id -> Api.Data.Entity -> List KVTableRow
apiEntityToRows clstrid clstr =
    [ Gap
    , Row "Number of Addresses" (ValueInt clstr.noAddresses)
    , Row "Total received" (Currency clstr.totalReceived (Asset.assetFromBase clstr.currency))
    , Row "Total sent" (Currency clstr.totalSpent (Asset.assetFromBase clstr.currency))
    , Row "Balance" (Currency clstr.balance (Asset.assetFromBase clstr.currency))
    , Gap
    , Row "First usage" (Timestamp clstr.firstTx.timestamp)
    , Row "Last usage" (Timestamp clstr.lastTx.timestamp)
    , LinkRow "more..."
        (Route.Graph.entityRoute { currency = Id.network clstrid, entity = Id.id clstrid |> Hex.fromString |> Result.withDefault 0, layer = Nothing, table = Nothing }
            |> Route.Graph
            |> Route.toUrl
        )
    ]


clusterInfoView : View.Config -> Bool -> Colors.ScopedColorAssignment -> Int -> Api.Data.Entity -> Html Msg
clusterInfoView vc open colors _ clstr =
    if clstr.noAddresses <= 1 then
        none

    else
        let
            clstrid =
                Id.initClusterId clstr.currency clstr.entity

            openIcon =
                FontAwesome.icon FontAwesome.minus |> Html.fromUnstyled

            --inlineChevronUpThinIcon
            closeIcon =
                FontAwesome.icon FontAwesome.plus |> Html.fromUnstyled

            --inlineChevronDownThinIcon
            clusterColor =
                Colors.getAssignedColor Colors.Clusters clstrid colors

            clusterIcon =
                clusterColor
                    |> Maybe.map (.color >> inlineClusterIcon vc.highlightClusterFriends)
                    |> Maybe.withDefault none
        in
        div [ css [ Css.color Css.lightGreyColor, Css.width (Css.pct 100) ] ]
            [ div [ css [ Css.paddingLeft (Css.px 8), Css.paddingBottom Css.mGap, Css.color Css.lightGreyColor, Css.displayFlex, Css.justifyContent Css.spaceBetween, Css.alignItems Css.center, Css.cursor Css.pointer ], Svg.onClick UserClickedToggleClusterDetailsOpen ]
                [ div
                    [ css
                        [ Css.displayFlex
                        , Css.alignItems Css.center
                        ]
                    ]
                    [ -- left sind of the bar
                      span [ css Css.smPaddingRight ]
                        [ if open then
                            openIcon

                          else
                            closeIcon
                        ]
                    , span [ css Css.smPaddingRight ] [ Locale.text vc.locale "Cluster Information" ]
                    , if vc.highlightClusterFriends then
                        span [ css Css.smPaddingRight, HA.title (Id.id clstrid), css [ Css.display Css.inline ] ] [ clusterIcon ]

                      else
                        none
                    ]
                ]
            , if open then
                div [ css [ Css.fontSize (Css.px 12), Css.color Css.lightGreyColor, Css.marginLeft (Css.px 8) ] ]
                    [ detailsFactTableView vc (apiEntityToRows clstrid clstr)
                    ]

              else
                none
            ]


detailsFactTableView : View.Config -> List KVTableRow -> Html Msg
detailsFactTableView vc rows =
    div [ Css.smPaddingBottom |> css ] [ renderKVTable vc rows ]


primaryButton : View.Config -> BtnConfig -> Html Msg
primaryButton vc btn =
    optionalTextButton vc Css.Primary btn


secondaryButton : View.Config -> BtnConfig -> Html Msg
secondaryButton vc btn =
    optionalTextButton vc Css.Secondary btn


optionalTextButton : View.Config -> Css.ButtonType -> BtnConfig -> Html Msg
optionalTextButton vc bt btn =
    let
        ( iconattr, content ) =
            if String.isEmpty btn.text then
                ( [], [] )

            else
                ( [ (Css.smPaddingRight ++ [ Css.verticalAlign Css.middle ]) |> css ], [ Html.text (Locale.string vc.locale btn.text) ] )
    in
    disableableButton (Css.detailsActionButtonStyle vc bt)
        btn
        []
        (span iconattr [ btn.icon btn.enable ]
            :: content
        )


graphSvg : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> BBox -> Svg Msg
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
                [ id name
                ]
                [ stop
                    [ from
                        |> Color.toCssString
                        |> stopColor
                    ]
                    []
                , stop
                    [ offset "70%"
                    , to
                        |> Color.toCssString
                        |> stopColor
                    ]
                    []
                ]

        originShiftX =
            Css.searchBoxMinWidth / 2
    in
    svg
        ([ preserveAspectRatio "xMidYMid meet"
         , model.transform
            |> Transform.update { x = 0, y = 0 } { x = -originShiftX, y = 0 }
            |> Transform.viewBox dim
            |> viewBox
         , (Css.Graph.svgRoot vc ++ pointerStyle) |> css
         , UserClickedGraph model.dragging
            |> Svg.onClick
         , id "graph"
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
            [ gradient "utxoOutEdgeForth" Colors.pathMiddle Colors.pathOut
            , gradient "utxoInEdgeForth" Colors.pathIn Colors.pathMiddle
            , gradient "utxoOutEdgeBack" Colors.pathOut Colors.pathMiddle
            , gradient "utxoInEdgeBack" Colors.pathMiddle Colors.pathIn
            , gradient "accountOutEdgeForth" Colors.pathIn Colors.pathOut
            , gradient "accountInEdgeForth" Colors.pathOut Colors.pathIn
            , gradient "accountOutEdgeBack" Colors.pathOut Colors.pathIn
            , gradient "accountInEdgeBack" Colors.pathOut Colors.pathIn
            ]
        , Svg.lazy6 Network.addresses plugins vc gc model.colors model.clusters model.network.addresses
        , Svg.lazy4 Network.txs plugins vc gc model.network.txs
        , Svg.lazy5 Network.edges plugins vc gc model.network.addresses model.network.txs
        , drawDragSelector vc model

        -- , rect [ fill "red", width "3", height "3", x "0", y "0" ] [] -- Mark zero point in coordinate system
        -- , showBoundingBox model
        ]



-- showBoundingBox : Model -> Svg Msg
-- showBoundingBox model =
--     let
--         bb =
--             Network.getBoundingBox model.network
--     in
--     rect [ fill "red", width (bb.width * unit + (2 * unit) |> String.fromFloat), height (bb.height * unit + (2 * unit) |> String.fromFloat), x (bb.x * unit - unit |> String.fromFloat), y ((bb.y * unit - unit) |> String.fromFloat) ] []


drawDragSelector : View.Config -> Pathfinder.Model -> Svg Msg
drawDragSelector _ m =
    case ( m.dragging, m.pointerTool ) of
        ( Dragging tm start now, Select ) ->
            let
                originShiftX =
                    Css.searchBoxMinWidth / 2

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
            in
            GraphComponents.selectionBoxWithAttributes
                (GraphComponents.selectionBoxAttributes
                    |> Rs.s_selectionBox
                        [ Util.Graph.translate xn yn |> transform
                        ]
                    |> Rs.s_rectangle
                        [ String.fromFloat widthn |> width
                        , String.fromFloat heightn |> height
                        ]
                )
                {}

        _ ->
            none


dateRangePickerSelectionView : View.Config -> DateRangePicker.Model AddressDetails.Msg -> Html Msg
dateRangePickerSelectionView vc model =
    let
        startP =
            model.fromDate

        endP =
            model.toDate

        -- selectedDuration =
        --     Locale.durationPosix vc.locale 1 startP endP
        startML =
            dateFromTimestamp vc (Locale.posixToTimestampSeconds startP)

        endML =
            dateFromTimestamp vc (Locale.posixToTimestampSeconds endP)

        attr =
            [ css [ Css.cursor Css.pointer ], Svg.onClick (AddressDetailsMsg <| AddressDetails.OpenDateRangePicker) ]
    in
    div [ Css.dateTimeRangeBoxStyle vc |> css ]
        [ span attr [ HIcons.iconsCalendar {} ]

        -- , span [] [ Html.text selectedDuration ]
        , span ((Css.dateTimeRangeHighlightedDateStyle vc |> css) :: attr) [ startML ]
        , span attr [ Html.text (Locale.string vc.locale "to") ]
        , span ((Css.dateTimeRangeHighlightedDateStyle vc |> css) :: attr) [ endML ]
        , button [ Css.linkButtonStyle vc True |> css, (AddressDetailsMsg <| AddressDetails.ResetDateRangePicker) |> Svg.onClick ] [ HIcons.iconsCloseSmall {} ]
        ]


transactionTableView : View.Config -> Id -> (Id -> Bool) -> TransactionTable.Model -> Html Msg
transactionTableView vc addressId txOnGraphFn model =
    let
        prevMsg =
            \_ -> AddressDetailsMsg AddressDetails.UserClickedPreviousPageTransactionTable

        nextMsg =
            \_ -> AddressDetailsMsg AddressDetails.UserClickedNextPageTransactionTable

        firstMsg =
            \_ -> AddressDetailsMsg AddressDetails.UserClickedFirstPageTransactionTable

        styles =
            Css.Table.styles

        table =
            PagedTable.pagedTableView vc
                []
                (TransactionTable.config styles vc addressId txOnGraphFn)
                model.table
                prevMsg
                nextMsg
                firstMsg

        filterRow drp =
            div
                [ css
                    [ Css.displayFlex
                    , Css.justifyContent Css.spaceBetween
                    , Css.marginBottom Css.lGap
                    , Css.marginTop Css.mGap
                    , Css.marginRight Css.sGap
                    ]
                ]
                [ drp
                , graphActionButton vc (BtnConfig (\_ -> HIcons.iconsFilterWithAttributes { iconsFilter = [ css [ Css.padding Css.no ] ], filter = [ css [ Css.fill Css.lightGreyColor |> Css.important ] ] } {}) "" (AddressDetailsMsg <| AddressDetails.OpenDateRangePicker) True)
                ]

        showSelectionRow =
            (model.txMaxBlock /= Nothing) || (model.txMinBlock /= Nothing)
    in
    (case model.dateRangePicker of
        Just drp ->
            if DatePicker.isOpen drp.dateRangePicker then
                [ span [ css [ Css.paddingLeft Css.mlGap ] ]
                    [ primaryButton vc (BtnConfig (\_ -> inlineDoneSmallIcon) "Ok" (AddressDetailsMsg <| AddressDetails.CloseDateRangePicker) True)
                    , secondaryButton vc (BtnConfig (\_ -> inlineCloseSmallIcon) "Reset Filter" (AddressDetailsMsg <| AddressDetails.ResetDateRangePicker) True)
                    ]
                , span [ css [ Css.fontSize (Css.px 12) ] ]
                    [ DatePicker.view drp.settings drp.dateRangePicker
                        |> Html.fromUnstyled
                        |> Html.map AddressDetailsMsg
                    ]
                ]

            else
                [ (if showSelectionRow then
                    dateRangePickerSelectionView vc drp

                   else
                    none
                  )
                    |> filterRow
                , table
                ]

        Nothing ->
            [ filterRow none
            , table
            ]
    )
        |> div [ css [ Css.width (Css.pct 100) ] ]
