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
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (id, src)
import Html.Styled.Lazy exposing (..)
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
import Route.Graph
import Route.Pathfinder exposing (Route(..))
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as SA exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Lazy as Svg
import Theme.Colors as Colors
import Theme.Html.Icons
import Theme.Html.SidePanelComponents as SidebarComponents
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.Icons as Icons
import Update.Graph.Transform as Transform
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.Graph
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags)
import Util.View exposing (copyIcon, copyableLongIdentifierPathfinder, none, truncateLongIdentifierWithLengths)
import View.Graph.Table exposing (noTools)
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.Icons exposing (inIcon, outIcon)
import View.Pathfinder.Network as Network
import View.Pathfinder.PagedTable as PagedTable
import View.Pathfinder.Table.IoTable as IoTable
import View.Pathfinder.Table.NeighborsTable as NeighborsTable
import View.Pathfinder.Table.TransactionTable as TransactionTable
import View.Pathfinder.Tooltip as Tooltip
import View.Pathfinder.Utils exposing (dateFromTimestamp, multiLineDateTimeFromTimestamp)
import View.Search


type alias BtnConfig =
    { icon : Bool -> Html Msg, text : String, msg : Msg, enable : Bool }


inlineExportIcon : Html Msg
inlineExportIcon =
    Theme.Html.Icons.iconExportWithAttributes (Theme.Html.Icons.iconExportAttributes |> s_iconExport [ css [ Css.display Css.inline ] ]) {}


inlineCloseSmallIcon : Html Msg
inlineCloseSmallIcon =
    Theme.Html.Icons.iconsCloseSmallWithAttributes (Theme.Html.Icons.iconsCloseSmallAttributes |> s_iconsCloseSmall [ css [ Css.display Css.inline ] ]) {}


inlineDoneIcon : Html Msg
inlineDoneIcon =
    Theme.Html.Icons.iconsDoneWithAttributes (Theme.Html.Icons.iconsDoneAttributes |> s_iconsDone [ css [ Css.display Css.inline ] ]) {}


inlineTagLargeIcon : Html Msg
inlineTagLargeIcon =
    Theme.Html.Icons.iconsTagLargeWithAttributes (Theme.Html.Icons.iconsTagLargeAttributes |> s_iconsTagLarge [ css [ Css.display Css.inline ] ]) {}


inlineClusterIcon : Bool -> Color -> Html Msg
inlineClusterIcon highlight clr =
    let
        getHighlight c =
            if highlight then
                [ css ((Util.View.toCssColor >> Css.fill >> Css.important >> List.singleton) c) ]

            else
                []
    in
    Theme.Html.Icons.iconsClusterWithAttributes (Theme.Html.Icons.iconsClusterAttributes |> s_iconsCluster [ css [ Css.display Css.inline ] ] |> s_vector (getHighlight clr)) {}


inlineChevronRightThickIcon : Html Msg
inlineChevronRightThickIcon =
    Theme.Html.Icons.iconsChevronRightThickWithAttributes (Theme.Html.Icons.iconsChevronRightThickAttributes |> s_iconsChevronRightThick [ css [ Css.display Css.inline ] ]) {}


inlineChevronDownThickIcon : Html Msg
inlineChevronDownThickIcon =
    Theme.Html.Icons.iconsChevronDownThickWithAttributes (Theme.Html.Icons.iconsChevronDownThickAttributes |> s_iconsChevronDownThick [ css [ Css.display Css.inline ] ]) {}


inlineChevronDownThinIcon : Html Msg
inlineChevronDownThinIcon =
    Theme.Html.Icons.iconsChevronDownThinWithAttributes (Theme.Html.Icons.iconsChevronDownThinAttributes |> s_iconsChevronDownThin [ css [ Css.display Css.inline ] ]) {}


inlineChevronUpThinIcon : Html Msg
inlineChevronUpThinIcon =
    Theme.Html.Icons.iconsChevronUpThinWithAttributes (Theme.Html.Icons.iconsChevronUpThinAttributes |> s_iconsChevronUpThin [ css [ Css.display Css.inline ] ]) {}


graphActionTools : Model -> List BtnConfig
graphActionTools m =
    [ BtnConfig (\_ -> Theme.Html.Icons.iconsNewFile {}) "restart" UserClickedRestart m.isDirty
    , BtnConfig (\_ -> Theme.Html.Icons.iconsOpen {}) "open" NoOp False
    , BtnConfig
        (\enabled ->
            if not enabled then
                Theme.Html.Icons.iconsRedoStateDisabled {}

            else
                Theme.Html.Icons.iconsRedoStateActive {}
        )
        "redo"
        UserClickedRedo
        (not (List.isEmpty m.history.future))
    , BtnConfig (\_ -> Theme.Html.Icons.iconsUndo {}) "undo" UserClickedUndo (not (List.isEmpty m.history.past))
    ]


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
                , td [] [ Html.a [ HA.href l, Css.View.link vc |> css ] [ Html.text n ] ]
                ]


renderValueTypeValue : View.Config -> ValueType -> Html Msg
renderValueTypeValue vc val =
    case val of
        ValueInt v ->
            span [] [ Html.text (Locale.int vc.locale v) ]

        ValueHex v ->
            span [] [ Html.text (Hex.toString v) ]

        InOut total inv outv ->
            inOutIndicator vc total inv outv

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


inOutIndicator : View.Config -> Maybe Int -> Int -> Int -> Html Msg
inOutIndicator vc mnr inNr outNr =
    let
        prefix =
            String.trim (String.join " " [ mnr |> Maybe.map (Locale.int vc.locale) |> Maybe.withDefault "", "(" ])
    in
    span [ ioOutIndicatorStyle |> toAttr ] [ Html.text prefix, inIcon, Html.text (Locale.int vc.locale inNr), Html.text ",", outIcon, Html.text (Locale.int vc.locale outNr), Html.text ")" ]


collapsibleSection : View.Config -> String -> Bool -> Maybe (Html Msg) -> Html Msg -> Msg -> Html Msg
collapsibleSection vc =
    collapsibleSectionRaw (collapsibleSectionHeadingStyle vc |> toAttr) (collapsibleSectionIconStyle |> toAttr) vc


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
    div []
        (div [ headingAttr, onClick action ]
            [ span [ iconAttr ] [ icon ]
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
        utc_text =
            if vc.showDatesInUserLocale then
                "User"

            else
                "UTC"

        content =
            div []
                [ div [ panelHeadingStyle3 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Transaction") ]
                , Util.View.onOffSwitch vc [ HA.checked vc.showTimestampOnTxEdge, onClick (UserClickedToggleShowTxTimestamp |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "Show timestamp")
                , div [ panelHeadingStyle3 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Date") ]
                , Util.View.onOffSwitch vc [ HA.checked vc.showDatesInUserLocale, onClick (UserClickedToggleDatesInUserLocale |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale utc_text)
                , Util.View.onOffSwitch vc [ HA.checked vc.showTimeZoneOffset, onClick (UserClickedToggleShowTimeZoneOffset |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "Show timezone")
                , div [ panelHeadingStyle3 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Cluster") ]
                , Util.View.onOffSwitch vc [ HA.checked vc.highlightClusterFriends, onClick (UserClickedToggleHighlightClusterFriends |> ChangedDisplaySettingsMsg) ] (Locale.string vc.locale "Highlight clusters")
                ]
    in
    div [ boxStyle vc Nothing |> toAttr ]
        [ collapsibleSectionRaw (collapsibleSectionHeadingDisplaySettingsStyle vc |> toAttr) (collapsibleSectionDisplaySettingsIconStyle |> toAttr) vc "Settings" m.config.isDisplaySettingsOpen Nothing content (ChangedDisplaySettingsMsg UserClickedToggleDisplaySettings)
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
            BtnConfig (\_ -> Theme.Html.Icons.iconsMouseCursor {}) "Selection Tool" (ChangePointerTool Select |> ChangedDisplaySettingsMsg) True

        dragBtn =
            BtnConfig (\_ -> Theme.Html.Icons.iconsHand {}) "Move Tool" (ChangePointerTool Drag |> ChangedDisplaySettingsMsg) True
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
            [ div [ toolIconStyle vc |> toAttr ] [ btn.icon btn.enable ]
            ]
        ]


graphToolButton : View.Config -> BtnConfig -> Svg Msg
graphToolButton vc btn =
    div [ toolItemStyle vc |> toAttr ]
        [ disableableButton (toolButtonStyle vc)
            btn
            []
            [ div [ toolIconStyle vc |> toAttr ] [ btn.icon btn.enable ]
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
    disableableButton (graphActionButtonStyle vc) btn [] (iconWithText vc (btn.icon True) (Locale.string vc.locale btn.text))


iconWithText : View.Config -> Html Msg -> String -> List (Html Msg)
iconWithText _ faIcon text =
    [ span [ iconWithTextStyle |> toAttr ] [ faIcon ], Html.text text ]


searchBoxView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
searchBoxView plugins _ vc _ model =
    div
        [ searchBoxStyle vc Nothing |> toAttr ]
        [ div [ panelHeadingStyle2 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Search") ]
        , div [ searchBoxContainerStyle vc |> toAttr ]
            [ span [ searchBoxIconStyle vc |> toAttr ] [ Theme.Html.Icons.iconsSearchLarge {} ]
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
    button [ linkButtonStyle vc True |> toAttr, msg |> onClick ] [ Theme.Html.Icons.iconsCloseSmall {} ]


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
        [ BtnConfig (\_ -> Theme.Html.Icons.iconsSettings {}) (Locale.string vc.locale "is contract") NoOp True ]

    else
        []


getAddressActionBtns : Id -> Api.Data.Address -> List BtnConfig
getAddressActionBtns _ _ =
    []


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
            Just (inOutIndicator vc Nothing data.noOutputs data.noInputs)
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

        nrTagsAddress =
            ts |> Maybe.map .tagCount |> Maybe.withDefault 0

        tags =
            ts
                |> Maybe.map
                    (\x ->
                        if hasOnlyExchangeTags x then
                            []

                        else
                            (.labelTagCloud >> Dict.toList >> List.sortBy (Tuple.second >> .weighted)) x
                    )
                |> Maybe.withDefault []

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

        clstrId =
            ( viewState.data.currency, Hex.toString viewState.data.entity )

        clstr =
            Dict.get clstrId model.clusters

        tbls =
            [ detailsFactTableView vc (apiAddressToRows viewState.data)
            , clusterInfoView vc model.config.isClusterDetailsOpen model.colors nrTagsAddress clstrId clstr
            , detailsActionsView vc (getAddressActionBtns id viewState.data)
            ]

        -- addressAnnotationBtns =
        --     getAddressAnnotationBtns vc viewState.data actor (Dict.member id model.tagSummaries)
        df =
            SidebarComponents.sidePanelHeaderAttributes |> s_sidePanelHeader [ css [ Css.alignItems Css.start |> Css.important ] ]

        inst =
            SidebarComponents.sidePanelHeaderInstances

        showExchangeTag =
            actorText /= Nothing

        showOtherTag =
            List.isEmpty tags |> not
    in
    SidebarComponents.sidePanelHeaderWithInstances
        df
        { inst
            | sidePanelHeaderTags =
                if showExchangeTag || showOtherTag then
                    Nothing

                else
                    Just none
        }
        { sidePanelHeader =
            { headerInstance =
                SidebarComponents.sidePanelAddressHeaderWithAttributes
                    (SidebarComponents.sidePanelAddressHeaderAttributes |> s_sidePanelAddressHeader [ css [ Css.padding (Css.px 0) ] ])
                    { sidePanelAddressHeader =
                        { iconInstance =
                            if address.exchange /= Nothing then
                                Icons.iconsExchangeSvg [] {}

                            else
                                Icons.iconsUntaggedSvg [] {}
                        , headerText =
                            (String.toUpper <| Id.network id) ++ " " ++ Locale.string vc.locale "address"
                        }
                    , addressLabelCopyIcon =
                        { iconInstance = Id.id id |> copyIcon vc
                        , text = Id.id id |> truncateLongIdentifierWithLengths 8 4
                        }
                    }
            }
        , sidePanelHeaderTags =
            { exchangeTagVisible = showExchangeTag
            , otherTagVisible = showOtherTag
            }
        , tagsLabel =
            { iconInstance = Icons.iconsTagLargeSvg [] {}
            , text = String.join ", " tagsDisplayWithMore
            }
        , actorLabel =
            { iconInstance =
                let
                    iconDetails =
                        Theme.Html.Icons.iconsAssignDetails

                    icon =
                        Icons.iconsAssignSvg
                in
                actorImg
                    |> Maybe.map
                        (\imgSrc ->
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
                    |> Maybe.withDefault (icon [] {})
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
            Just (inOutIndicator vc (Just (data.noIncomingTxs + data.noOutgoingTxs)) data.noIncomingTxs data.noOutgoingTxs)
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
                , PagedTable.pagedTableView vc attributes tblCfg viewState.neighborsOutgoing (prevMsg Outgoing) (nextMsg Outgoing)
                , h2 [ panelHeadingStyle2 vc |> toAttr ] [ Html.text "Incoming" ]
                , PagedTable.pagedTableView vc attributes tblCfg viewState.neighborsIncoming (prevMsg Incoming) (nextMsg Incoming)
                ]

        ioIndicatorState =
            Just (inOutIndicator vc Nothing data.inDegree data.outDegree)
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
    disableableButton (linkButtonStyle vc) btn [ HA.title btn.text ] [ btn.icon btn.enable ]


apiAddressToRows : Api.Data.Address -> List KVTableRow
apiAddressToRows address =
    [ Row "Total received" (Currency address.totalReceived address.currency)
    , Row "Total sent" (Currency address.totalSpent address.currency)
    , Row "Balance" (Currency address.balance address.currency)
    , Gap
    , Row "First usage" (TimestampWithTime address.firstTx.timestamp)
    , Row "Last usage" (TimestampWithTime address.lastTx.timestamp)
    ]


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


clusterInfoView : View.Config -> Bool -> Colors.ScopedColorAssignment -> Int -> Id -> Maybe Api.Data.Entity -> Html Msg
clusterInfoView vc open colors nrAddessTags clstrid mcluster =
    case mcluster of
        Just clstr ->
            if clstr.noAddresses > 1 then
                let
                    openIcon =
                        FontAwesome.icon FontAwesome.minus |> Html.fromUnstyled

                    --inlineChevronUpThinIcon
                    closeIcon =
                        FontAwesome.icon FontAwesome.plus |> Html.fromUnstyled

                    --inlineChevronDownThinIcon
                    clusterColor =
                        Colors.getAssignedColor Colors.Clusters clstrid colors

                    clusterIcon =
                        clusterColor |> Maybe.map (.color >> inlineClusterIcon vc.highlightClusterFriends) |> Maybe.withDefault none
                in
                div [ css [ Css.color Css.lightGreyColor, Css.cursor Css.pointer ] ]
                    [ span [ css [ Css.paddingLeft (Css.px 8), Css.color Css.lightGreyColor ], onClick UserClickedToggleClusterDetailsOpen ]
                        [ span [ css Css.smPaddingRight ]
                            [ if open then
                                openIcon

                              else
                                closeIcon
                            ]
                        , span [ css Css.smPaddingRight ] [ Locale.text vc.locale "Cluster Information" ]
                        , span [ css [ Css.float Css.right, Css.paddingRight (Css.px 15) ] ]
                            [ if vc.highlightClusterFriends then
                                span [ css Css.smPaddingRight, HA.title (Id.id clstrid) ] [ clusterIcon ]

                              else
                                none
                            , if clstr.noAddressTags > nrAddessTags then
                                span [ HA.title (Locale.string vc.locale "Cluster has addidional tags") ] [ inlineTagLargeIcon ]

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

            else
                none

        _ ->
            none


apiUtxoTxToRows : Api.Data.TxUtxo -> List KVTableRow
apiUtxoTxToRows tx =
    [ Row "Timestamp" (TimestampWithTime tx.timestamp)
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

        selectedDuration =
            Locale.durationPosix vc.locale 1 startP endP

        startML =
            dateFromTimestamp vc (Locale.posixToTimestampSeconds startP)

        endML =
            dateFromTimestamp vc (Locale.posixToTimestampSeconds endP)
    in
    div [ dateTimeRangeBoxStyle vc |> toAttr ]
        [ Theme.Html.Icons.iconsCalendar {}
        , span [] [ Html.text selectedDuration ]
        , span [ dateTimeRangeHighlightedDateStyle vc |> toAttr ] [ startML ]
        , span [] [ Html.text (Locale.string vc.locale "to") ]
        , span [ dateTimeRangeHighlightedDateStyle vc |> toAttr ] [ endML ]
        , button [ linkButtonStyle vc True |> toAttr, (AddressDetailsMsg <| AddressDetails.ResetDateRangePicker) |> onClick ] [ Theme.Html.Icons.iconsCloseSmall {} ]
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
            PagedTable.pagedTableView vc
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
                , secondaryButton vc (BtnConfig (\_ -> Theme.Html.Icons.iconsFilter {}) "" (AddressDetailsMsg <| AddressDetails.OpenDateRangePicker) True)
                ]

        showSelectionRow =
            (model.txMaxBlock /= Nothing) || (model.txMinBlock /= Nothing)
    in
    (case model.dateRangePicker of
        Just drp ->
            if DatePicker.isOpen drp.dateRangePicker then
                [ span []
                    [ primaryButton vc (BtnConfig (\_ -> inlineDoneIcon) "Ok" (AddressDetailsMsg <| AddressDetails.CloseDateRangePicker) True)
                    , secondaryButton vc (BtnConfig (\_ -> inlineCloseSmallIcon) "Reset Filter" (AddressDetailsMsg <| AddressDetails.ResetDateRangePicker) True)
                    ]
                , DatePicker.view drp.settings drp.dateRangePicker
                    |> Html.fromUnstyled
                    |> Html.map AddressDetailsMsg
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
