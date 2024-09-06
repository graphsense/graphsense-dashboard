module View.Pathfinder exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Browser.Events exposing (Visibility(..))
import Color exposing (Color)
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
import Hex
import Hovercard
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (id, src)
import Html.Styled.Events exposing (onMouseEnter, onMouseLeave)
import Html.Styled.Lazy exposing (..)
import Init.Pathfinder.Id as Id
import Json.Decode
import Model.Currency exposing (Currency(..), assetFromBase)
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (BBox, Coords)
import Model.Graph.Table
import Model.Graph.Transform exposing (Transition(..))
import Model.Pathfinder exposing (..)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Colors as Colors
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
import RecordSetter exposing (..)
import RemoteData
import Route
import Route.Graph exposing (AddressTable(..))
import Route.Pathfinder exposing (Route(..))
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as SA exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Lazy as Svg
import Theme.Colors as Colors
import Theme.Html.Icons as HIcons
import Theme.Html.SettingsComponents as SettingsComponents
import Theme.Html.SidePanelComponents as SidePanelComponents
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.Icons as Icons
import Update.Graph.Transform as Transform
import Util.Css as Css
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.Graph
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags)
import Util.View exposing (copyIconPathfinder, hovercard, none, truncateLongIdentifierWithLengths)
import View.Graph.Table exposing (noTools)
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.Icons exposing (inIcon, outIcon)
import View.Pathfinder.Network as Network
import View.Pathfinder.PagedTable as PagedTable
import View.Pathfinder.Table.IoTable as IoTable
import View.Pathfinder.Table.NeighborsTable as NeighborsTable
import View.Pathfinder.Table.TransactionTable as TransactionTable
import View.Pathfinder.Toolbar as Toolbar
import View.Pathfinder.Tooltip as Tooltip
import View.Pathfinder.Utils exposing (dateFromTimestamp, multiLineDateTimeFromTimestamp)
import View.Search


type alias BtnConfig =
    { icon : Bool -> Html Msg, text : String, msg : Msg, enable : Bool }


inlineExportIcon : Html Msg
inlineExportIcon =
    HIcons.iconExportWithAttributes HIcons.iconExportAttributes {}


inlineCloseSmallIcon : Html Msg
inlineCloseSmallIcon =
    HIcons.iconsCloseSmallWithAttributes HIcons.iconsCloseSmallAttributes {}


inlineDoneSmallIcon : Html Msg
inlineDoneSmallIcon =
    HIcons.iconsDoneSmallWithAttributes HIcons.iconsDoneSmallAttributes {}


inlineTagLargeIcon : Html Msg
inlineTagLargeIcon =
    HIcons.iconsTagLargeWithAttributes (HIcons.iconsTagLargeAttributes |> s_iconsTagLarge [ css [ Css.display Css.inline ] ]) {}


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
            |> s_ellipse25 (getHighlight clr)
        )
        {}


inlineChevronRightThickIcon : Html Msg
inlineChevronRightThickIcon =
    HIcons.iconsChevronRightThick {}


inlineChevronDownThickIcon : Html Msg
inlineChevronDownThickIcon =
    HIcons.iconsChevronDownThick {}


graphActionButtons : List BtnConfig
graphActionButtons =
    [ BtnConfig (\_ -> inlineExportIcon) "Export" (UserClickedExportGraphAsPNG "graph") True
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
    | LinkRow String String
    | Gap


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

        LinkRow n l ->
            tr []
                [ td [ kVTableKeyTdStyle vc |> toAttr ] []
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

        Currency v ticker ->
            span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.currencyWithoutCode2 vc.locale [ ( assetFromBase ticker, v ) ]) ]

        CurrencyWithCode v ticker ->
            -- span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.coinWithoutCode vc.locale (assetFromBase ticker) v.value ++ " " ++ ticker) ]
            span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.currencyWithoutCode vc.locale [ ( assetFromBase ticker, v ) ]) ]

        CopyIdent ident ->
            Util.View.copyableLongIdentifierPathfinder vc [] ident

        Timestamp ts ->
            span [] [ Locale.timestampDateUniform vc.locale ts |> Html.text ]

        TimestampWithTime ts ->
            span [] [ multiLineDateTimeFromTimestamp vc ts ]


renderValueTypeExtension : View.Config -> ValueType -> Html Msg
renderValueTypeExtension vc val =
    case val of
        Currency _ ticker ->
            span []
                [ Html.text
                    (String.toUpper
                        (case vc.locale.currency of
                            Coin ->
                                ticker

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
                [ btn.msg |> onClick ]

            else
                [ HA.disabled True ]
    in
    button (((style btn.enable |> toAttr) :: addattr) ++ attrs) content


rule : Html Msg
rule =
    hr [ ruleStyle |> toAttr ] []


inOutIndicator : View.Config -> String -> Int -> Int -> Int -> Html Msg
inOutIndicator vc title mnr inNr outNr =
    SidePanelComponents.sidePanelListHeaderContent
        { sidePanelListHeaderContent =
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
    span [ ioOutIndicatorStyle |> toAttr ] [ Html.text prefix, inIcon, Html.text (Locale.int vc.locale inNr), outIcon, Html.text (Locale.int vc.locale outNr), Html.text ")" ]


collapsibleSection : View.Config -> String -> Bool -> Maybe (Html Msg) -> Html Msg -> Msg -> Html Msg
collapsibleSection vc =
    collapsibleSectionRaw (collapsibleSectionHeadingStyle vc |> toAttr) ([] |> toAttr) vc


collapsibleSectionRaw : Html.Attribute Msg -> Html.Attribute Msg -> View.Config -> String -> Bool -> Maybe (Html Msg) -> Html Msg -> Msg -> Html Msg
collapsibleSectionRaw headingAttr iconAttr vc title open indicator content action =
    let
        icon =
            if open then
                inlineChevronDownThickIcon

            else
                inlineChevronRightThickIcon

        data =
            if open then
                [ content ]

            else
                []
    in
    div [ css [ Css.width <| Css.pct 100 ] ]
        (div [ headingAttr, onClick action ]
            [ span [ iconAttr ] [ icon ]
            , span [ iconAttr ] [ Html.text (Locale.string vc.locale title) ]
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


topCenterPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
topCenterPanel plugins ms vc gc model =
    div
        [ css topPanelStyle
        ]
        [ h2 [ vc.theme.heading2 |> toAttr ] [ Html.text "Pathfinder" ]
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
                , deleteDisabled = model.selection == NoSelection
                , pointerTool = model.pointerTool
                }
            ]
        , div
            [ css [ Css.property "pointer-events" "all" ] ]
            [ graphActionsView vc gc model
            ]
        ]


topLeftPanel : View.Config -> Html Msg
topLeftPanel vc =
    div [ topLeftPanelStyle vc |> toAttr ]
        []


settingsView : View.Config -> Model -> Hovercard.Model -> Html Msg
settingsView vc _ hc =
    let
        utc_text =
            if vc.showDatesInUserLocale then
                "User"

            else
                "UTC"
    in
    div [ css [ Css.padding Css.mlGap ] ]
        [ div [ panelHeadingStyle3 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Transaction") ]
        , Util.View.onOffSwitch vc [ HA.checked vc.showTimestampOnTxEdge, onClick (UserClickedToggleShowTxTimestamp |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "Show timestamp")
        , div [ panelHeadingStyle3 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Date") ]
        , Util.View.onOffSwitch vc [ HA.checked vc.showDatesInUserLocale, onClick (UserClickedToggleDatesInUserLocale |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale utc_text)
        , Util.View.onOffSwitch vc [ HA.checked vc.showTimeZoneOffset, onClick (UserClickedToggleShowTimeZoneOffset |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "Show timezone")
        , div [ panelHeadingStyle3 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Cluster") ]
        , Util.View.onOffSwitch vc [ HA.checked vc.highlightClusterFriends, onClick (UserClickedToggleHighlightClusterFriends |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "Highlight clusters")
        ]
        |> Html.toUnstyled
        |> List.singleton
        |> hovercard vc hc (Css.zIndexMainValue + 1)


topRightPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
topRightPanel _ _ vc gc model =
    div [ topRightPanelStyle vc |> toAttr ]
        [ detailsView vc gc model
        ]


graphActionsView : View.Config -> Pathfinder.Config -> Model -> Html Msg
graphActionsView vc _ _ =
    div [ graphActionsViewStyle vc |> toAttr ]
        (graphActionButtons |> List.map (graphActionButton vc))


graphActionButton : View.Config -> BtnConfig -> Html Msg
graphActionButton vc btn =
    disableableButton (graphActionButtonStyle vc) btn [] (iconWithText vc (btn.icon True) (Locale.string vc.locale btn.text))


iconWithText : View.Config -> Html Msg -> String -> List (Html Msg)
iconWithText _ faIcon text =
    [ span [ iconWithTextStyle |> toAttr ] [ faIcon ], Html.text text ]


searchBoxView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
searchBoxView plugins _ vc _ model =
    SettingsComponents.toolbarSearchFieldWithInstances
        (SettingsComponents.toolbarSearchFieldAttributes
            |> s_toolbarSearchField
                [ css [ Css.alignItems Css.stretch |> Css.important ] ]
        )
        (SettingsComponents.toolbarSearchFieldInstances
            |> s_searchInputField
                (View.Search.searchWithMoreCss plugins
                    vc
                    { css = searchInputStyle vc
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


detailsView : View.Config -> Pathfinder.Config -> Model -> Html Msg
detailsView vc gc model =
    case model.details of
        Just details ->
            case details of
                AddressDetails id state ->
                    RemoteData.unwrap
                        (Util.View.loadingSpinner vc Css.View.loadingSpinner)
                        (addressDetailsContentView vc gc model id)
                        state

                TxDetails id state ->
                    txDetailsContentView vc gc model id state

        Nothing ->
            none


closeButton : View.Config -> Msg -> Html Msg
closeButton vc msg =
    button [ linkButtonStyle vc True |> toAttr, msg |> onClick ] [ HIcons.iconsCloseSmall {} ]


getAddressAnnotationBtns : View.Config -> Api.Data.Address -> Maybe Api.Data.Actor -> Bool -> List BtnConfig
getAddressAnnotationBtns vc data _ _ =
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
        [ BtnConfig (\_ -> HIcons.iconsSettings {}) (Locale.string vc.locale "is contract") NoOp True ]

    else
        []


getAddressActionBtns : Id -> Api.Data.Address -> List BtnConfig
getAddressActionBtns _ _ =
    []


txDetailsContentView : View.Config -> Pathfinder.Config -> Model -> Id -> TxDetails.Model -> Html Msg
txDetailsContentView vc _ model id viewState =
    let
        getLbl id_ =
            Dict.get id_ model.tagSummaries
                |> Maybe.withDefault NoTags
    in
    SidePanelComponents.sidePanelComponent
        { actor = { iconInstance = none, text = "" }
        , leftTab = { tabLabel = "" }
        , rightTab = { tabLabel = "" }
        , sidePanelComponent =
            { detailsInstance =
                case viewState.tx.type_ of
                    Tx.Account tx ->
                        SidePanelComponents.sidePanelTxDetails
                            { titleOfInput = { text = "" }
                            , titleOfOutput = { text = "" }
                            , titleOfTimestamp = { text = Locale.string vc.locale "Timestamp" }
                            , valueOfInput = { firstRowText = "", secondRowText = "", secondRowVisible = False }
                            , valueOfOutput = { firstRowText = "", secondRowText = "", secondRowVisible = False }
                            , valueOfTimestamp = timeToCell vc tx.raw.timestamp
                            }

                    Tx.Utxo tx ->
                        SidePanelComponents.sidePanelTxDetails
                            { titleOfInput = { text = Locale.string vc.locale "Total input" }
                            , titleOfOutput = { text = Locale.string vc.locale "Total output" }
                            , titleOfTimestamp = { text = Locale.string vc.locale "Timestamp" }
                            , valueOfInput = valuesToCell vc tx.raw.currency tx.raw.totalInput
                            , valueOfOutput = valuesToCell vc tx.raw.currency tx.raw.totalOutput
                            , valueOfTimestamp = timeToCell vc tx.raw.timestamp
                            }
            , tableInstance =
                case viewState.tx.type_ of
                    Tx.Account _ ->
                        none

                    Tx.Utxo tx ->
                        utxoTxDetailsSectionsView vc model.network viewState tx.raw getLbl
            , tabsVisible = False
            }
        , sidePanelHeaderTags = { actorVisible = False, tagsVisible = False }
        , tags = { iconInstance = none, text = "" }
        , sidePanelHeader =
            { headerInstance =
                SidePanelComponents.sidePanelTxHeader
                    { sidePanelTxHeader =
                        { headerText =
                            (String.toUpper <| Id.network id) ++ " " ++ Locale.string vc.locale "Transaction"
                        }
                    , iconText =
                        { iconInstance = Id.id id |> copyIconPathfinder vc
                        , text = Id.id id |> truncateLongIdentifierWithLengths 8 4
                        }
                    }
            , tagInfoVisible = False
            }
        }


ioTableView : View.Config -> Network -> String -> Model.Graph.Table.Table Api.Data.TxValue -> (Id -> HavingTags) -> Html Msg
ioTableView vc network currency table getLbl =
    let
        isCheckedFn =
            flip Network.hasAddress network

        styles =
            Css.Table.styles
                |> s_root (\vc_ -> Css.Table.styles.root vc_ ++ [ Css.display Css.block ])
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
            Just (inOutIndicator vc "In- and Outputs" (data.noOutputs + data.noInputs) data.noOutputs data.noInputs)
    in
    collapsibleSection vc "" viewState.ioTableOpen ioIndicatorState content (TxDetailsMsg UserClickedToggleIOTable)


accountTxDetailsContentView : View.Config -> Api.Data.TxAccount -> Html Msg
accountTxDetailsContentView _ _ =
    div [] [ Html.text "I am a Account TX" ]


valuesToCell : View.Config -> String -> Api.Data.Values -> { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
valuesToCell vc currency value =
    { firstRowText = Locale.currency vc.locale [ ( assetFromBase currency, value ) ]
    , secondRowText = ""
    , secondRowVisible = False
    }


timeToCell : View.Config -> Int -> { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
timeToCell vc d =
    { firstRowText = Locale.timestampDateUniform vc.locale d
    , secondRowText = Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset d
    , secondRowVisible = True
    }


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

        nrTagsAddress =
            ts |> Maybe.map .tagCount |> Maybe.withDefault 0

        tagLabels =
            ts
                |> Maybe.map
                    (\x ->
                        if hasOnlyExchangeTags x then
                            []

                        else
                            (.labelSummary >> Dict.toList >> List.sortBy (Tuple.second >> .confidence) >> List.reverse) x
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

        tbls =
            [ --detailsFactTableView vc (apiAddressToRows viewState.data)
              SidePanelComponents.sidePanelDetails
                { balanceTitle = { text = Locale.string vc.locale "Balance" }
                , balanceValue = valuesToCell vc viewState.data.currency viewState.data.balance
                , totalReceivedTitle = { text = Locale.string vc.locale "Total received" }
                , totalReceivedValue = valuesToCell vc viewState.data.currency viewState.data.totalReceived
                , totalSentTitle = { text = Locale.string vc.locale "Total sent" }
                , totalSentValue = valuesToCell vc viewState.data.currency viewState.data.totalSpent
                , lastUsageTitle = { text = Locale.string vc.locale "Last usage" }
                , lastUsageValue = timeToCell vc viewState.data.lastTx.timestamp
                , firstUsageTitle = { text = Locale.string vc.locale "First usage" }
                , firstUsageValue = timeToCell vc viewState.data.firstTx.timestamp
                }
            , Dict.get clstrId model.clusters
                |> Maybe.map (clusterInfoView vc model.config.isClusterDetailsOpen model.colors nrTagsAddress)
                |> Maybe.withDefault none
            , detailsActionsView vc (getAddressActionBtns id viewState.data)
            ]

        -- addressAnnotationBtns =
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
                , HA.css SidePanelComponents.sidePanelComponentLabelOfTagsDetails.styles
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
                    Html.span [ Css.tagLinkButtonStyle vc |> css, HA.title (Locale.string vc.locale "show less..."), onClick UserClickedToggleDisplayAllTagsInDetails ] [ Html.text (Locale.string vc.locale "less...") ]

                else
                    Html.span [ Css.tagLinkButtonStyle vc |> css, HA.title (Locale.string vc.locale "show more..."), onClick UserClickedToggleDisplayAllTagsInDetails ] [ Html.text ("+" ++ String.fromInt (lenTagLabels - nMaxTags) ++ " "), Html.text (Locale.string vc.locale "more...") ]

            else
                none

        clstrid =
            Id.initClusterId viewState.data.currency viewState.data.entity

        clusterHighlightAttr =
            if vc.highlightClusterFriends then
                Colors.getAssignedColor Colors.Clusters clstrid model.colors
                    |> Maybe.map
                        (.color
                            >> Util.View.toCssColor
                            >> Css.fill
                            >> Css.important
                            >> List.singleton
                            >> css
                            >> List.singleton
                        )
                    |> Maybe.withDefault []

            else
                []
    in
    SidePanelComponents.sidePanelComponentWithInstances
        (SidePanelComponents.sidePanelComponentAttributes
            |> s_sidePanelComponent
                [ [ Css.calc (Css.vh 100) Css.minus (Css.px 150) |> Css.maxHeight
                  , Css.overflowY Css.auto
                  , Css.overflowX Css.hidden
                  ]
                    |> css
                ]
        )
        (SidePanelComponents.sidePanelComponentInstances
            |> s_labelOfTags
                (Just
                    (div
                        [ css
                            [ Css.displayFlex
                            , Css.flexDirection Css.row
                            , Css.flexWrap Css.wrap
                            , Css.property "gap" "1ex"
                            , Css.alignItems Css.center
                            , Css.maxWidth <| Css.pct 80
                            ]
                        ]
                        ((tagLabels |> List.take nTagsToShow |> List.indexedMap showTag) ++ [ tagsControl ])
                    )
                )
            |> s_labelOfActor
                (actor_id
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
                                , css SidePanelComponents.sidePanelComponentLabelOfTagsDetails.styles
                                ]
                                [ Html.text text
                                ]
                        )
                )
        )
        { actor =
            { iconInstance =
                actorImg
                    |> Maybe.map
                        (\imgSrc ->
                            let
                                iconDetails =
                                    HIcons.iconsAssignDetails
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
            , text = ""
            }
        , leftTab = { tabLabel = "" }
        , rightTab = { tabLabel = "" }
        , sidePanelComponent =
            { detailsInstance =
                div [] tbls
            , tableInstance = addressTransactionTableView vc gc id viewState txOnGraphFn
            , tabsVisible = False
            }
        , sidePanelHeaderTags = { actorVisible = showExchangeTag, tagsVisible = showOtherTag }
        , tags = { iconInstance = Icons.iconsTagLargeSvg [] {}, text = "" }
        , sidePanelHeader =
            { headerInstance =
                SidePanelComponents.sidePanelAddressHeader
                    { sidePanelAddressHeader =
                        { iconInstance =
                            if address.exchange /= Nothing then
                                Icons.iconsExchangeWithAttributesSvg []
                                    (Icons.iconsExchangeAttributes
                                        |> s_dollar clusterHighlightAttr
                                        |> s_arrows clusterHighlightAttr
                                    )
                                    {}

                            else
                                Icons.iconsUntaggedWithAttributesSvg []
                                    (Icons.iconsUntaggedAttributes
                                        |> s_ellipse25 clusterHighlightAttr
                                    )
                                    {}
                        , headerText =
                            (String.toUpper <| Id.network id) ++ " " ++ Locale.string vc.locale "address"
                        }
                    , addressLabelCopyIcon =
                        { iconInstance = Id.id id |> copyIconPathfinder vc
                        , text = Id.id id |> truncateLongIdentifierWithLengths 8 4
                        }
                    }
            , tagInfoVisible = showOtherTag || showExchangeTag
            }
        }


addressTransactionTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetails.Model -> (Id -> Bool) -> Html Msg
addressTransactionTableView vc _ addressId viewState txOnGraphFn =
    let
        data =
            viewState.data

        content =
            transactionTableView vc addressId txOnGraphFn viewState.txs

        ioIndicatorState =
            Just (inOutIndicator vc "Transactions" (data.noIncomingTxs + data.noOutgoingTxs) data.noIncomingTxs data.noOutgoingTxs)
    in
    collapsibleSection vc "" viewState.transactionsTableOpen ioIndicatorState content (AddressDetailsMsg AddressDetails.UserClickedToggleTransactionTable)


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
                , PagedTable.pagedTableView vc attributes tblCfg viewState.neighborsOutgoing (prevMsg Outgoing) (nextMsg Outgoing)
                , h2 [ panelHeadingStyle2 vc |> toAttr ] [ Html.text "Incoming" ]
                , PagedTable.pagedTableView vc attributes tblCfg viewState.neighborsIncoming (prevMsg Incoming) (nextMsg Incoming)
                ]

        ioIndicatorState =
            Just (inOutIndicatorOld vc Nothing data.inDegree data.outDegree)
    in
    collapsibleSection vc "Neighbors" viewState.neighborsTableOpen ioIndicatorState content (AddressDetailsMsg AddressDetails.UserClickedToggleNeighborsTable)


annotationButton : View.Config -> BtnConfig -> Html Msg
annotationButton vc btn =
    disableableButton (linkButtonStyle vc) btn [ HA.title btn.text ] [ btn.icon btn.enable ]


apiEntityToRows : Id -> Api.Data.Entity -> List KVTableRow
apiEntityToRows clstrid clstr =
    [ Gap
    , Row "Number of Addresses" (ValueInt clstr.noAddresses)
    , Row "Total received" (Currency clstr.totalReceived clstr.currency)
    , Row "Total sent" (Currency clstr.totalSpent clstr.currency)
    , Row "Balance" (Currency clstr.balance clstr.currency)
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
        div [ css [ Css.color Css.lightGreyColor, Css.cursor Css.pointer ] ]
            [ div [ css [ Css.paddingLeft (Css.px 8), Css.color Css.lightGreyColor, Css.displayFlex, Css.justifyContent Css.spaceBetween, Css.alignItems Css.center ], onClick UserClickedToggleClusterDetailsOpen ]
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
                ( [ (smPaddingRight ++ [ Css.verticalAlign Css.middle ]) |> toAttr ], [ Html.text (Locale.string vc.locale btn.text) ] )
    in
    disableableButton (detailsActionButtonStyle vc bt)
        btn
        []
        (span iconattr [ btn.icon btn.enable ]
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
                        |> Color.toCssString
                        |> SA.stopColor
                    ]
                    []
                , stop
                    [ SA.offset "70%"
                    , to
                        |> Color.toCssString
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


showBoundingBox : Model -> Svg Msg
showBoundingBox model =
    let
        bb =
            Network.getBoundingBox model.network
    in
    rect [ fill "red", width (bb.width * unit + (2 * unit) |> String.fromFloat), height (bb.height * unit + (2 * unit) |> String.fromFloat), x (bb.x * unit - unit |> String.fromFloat), y ((bb.y * unit - unit) |> String.fromFloat) ] []


drawDragSelector : View.Config -> Model -> Svg Msg
drawDragSelector _ m =
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
            in
            GraphComponents.selectionBoxWithAttributes
                (GraphComponents.selectionBoxAttributes
                    |> s_selectionBox
                        [ Util.Graph.translate xn yn |> transform
                        ]
                    |> s_rectangle
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
            [ css [ Css.cursor Css.pointer ], onClick (AddressDetailsMsg <| AddressDetails.OpenDateRangePicker) ]
    in
    div [ dateTimeRangeBoxStyle vc |> toAttr ]
        [ span attr [ HIcons.iconsCalendar {} ]

        -- , span [] [ Html.text selectedDuration ]
        , span ((dateTimeRangeHighlightedDateStyle vc |> toAttr) :: attr) [ startML ]
        , span attr [ Html.text (Locale.string vc.locale "to") ]
        , span ((dateTimeRangeHighlightedDateStyle vc |> toAttr) :: attr) [ endML ]
        , button [ linkButtonStyle vc True |> toAttr, (AddressDetailsMsg <| AddressDetails.ResetDateRangePicker) |> onClick ] [ HIcons.iconsCloseSmall {} ]
        ]


transactionTableView : View.Config -> Id -> (Id -> Bool) -> TransactionTable.Model -> Html Msg
transactionTableView vc addressId txOnGraphFn model =
    let

        prevMsg =
            \_ -> AddressDetailsMsg AddressDetails.UserClickedPreviousPageTransactionTable

        nextMsg =
            \_ -> AddressDetailsMsg AddressDetails.UserClickedNextPageTransactionTable

        styles =
            Css.Table.styles

        table =
            PagedTable.pagedTableView vc
                []
                (TransactionTable.config styles vc addressId txOnGraphFn)
                model.table
                prevMsg
                nextMsg

        filterRow drp =
            div
                [ css
                    [ Css.displayFlex
                    , Css.justifyContent Css.spaceBetween
                    , Css.marginBottom Css.lGap
                    ]
                ]
                [ drp
                , secondaryButton vc (BtnConfig (\_ -> HIcons.iconsFilterWithAttributes { iconsFilter = [ css [ Css.padding Css.no ] ], filter = [] } {}) "" (AddressDetailsMsg <| AddressDetails.OpenDateRangePicker) True)
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
        |> div []
