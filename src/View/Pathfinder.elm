module View.Pathfinder exposing (view)

import Api.Data
import Browser.Events exposing (Visibility(..))
import Components
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Graph
import Css.Pathfinder as Css exposing (..)
import Css.View
import Dict
import DurationDatePicker as DatePicker
import FontAwesome
import Html as HtmlDefault
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (id, src)
import Html.Styled.Lazy exposing (..)
import Json.Decode
import Model.Currency exposing (assetFromBase)
import Model.Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (BBox, Coords)
import Model.Graph.Transform exposing (Transition(..))
import Model.Pathfinder exposing (..)
import Model.Pathfinder.DatePicker exposing (userDefinedRangeDatePickerSettings)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Msg.Pathfinder exposing (AddressDetailsMsg(..), DisplaySettingsMsg(..), Msg(..), TxDetailsMsg(..))
import Number.Bounded exposing (value)
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import RemoteData
import Result.Extra
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as SA exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Lazy as Svg
import Table
import Time
import Util.Data exposing (negateTxValue)
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.Graph
import Util.Pathfinder exposing (getAddress)
import Util.View exposing (copyableLongIdentifier, none)
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.Error as Error
import View.Pathfinder.Icons exposing (inIcon, outIcon)
import View.Pathfinder.Network as Network
import View.Pathfinder.Table as Table
import View.Pathfinder.Table.IoTable as IoTable
import View.Pathfinder.Table.NeighborsTable as NeighborsTable
import View.Pathfinder.Table.TransactionTable as TransactionTable
import View.Search



-- Config


type alias BtnConfig =
    { icon : FontAwesome.Icon, text : String, msg : Msg, enable : Bool }


graphTools : List BtnConfig
graphTools =
    [ BtnConfig FontAwesome.trash "restart" UserClickedRestart True
    , BtnConfig FontAwesome.redo "redo" UserClickedRedo False
    , BtnConfig FontAwesome.undo "undo" UserClickedUndo True
    , BtnConfig FontAwesome.highlighter "highlight" UserClickedHighlighter True
    ]


graphActionButtons : List BtnConfig
graphActionButtons =
    [ BtnConfig FontAwesome.arrowUp "Import file" UserClickedImportFile True
    , BtnConfig FontAwesome.arrowDown "Export graph" UserClickedExportGraph True
    ]



-- Helpers


type ValueType
    = ValueInt Int
    | Text String
    | Currency Api.Data.Values String
    | CurrencyWithCode Api.Data.Values String
    | InOut (Maybe Int) Int Int
    | Timestamp Int
    | CopyIdent String


type KVTableRow
    = Row String ValueType
    | Gap



-- http://probablyprogramming.com/2009/03/15/the-tiniest-gif-ever


dummyImageSrc : View.Config -> String
dummyImageSrc _ =
    "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs="


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

        InOut total inv outv ->
            inOutIndicator total inv outv

        Text txt ->
            span [] [ Html.text txt ]

        Currency v ticker ->
            span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.coinWithoutCode vc.locale (assetFromBase ticker) v.value) ]

        CurrencyWithCode v ticker ->
            span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.coinWithoutCode vc.locale (assetFromBase ticker) v.value ++ " " ++ ticker) ]

        CopyIdent ident ->
            Util.View.copyableLongIdentifier vc [] ident

        Timestamp ts ->
            span [] [ Locale.timestampDateUniform vc.locale ts |> Html.text ]


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
collapsibleSection vc title open indicator content action =
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
        (div [ collapsibleSectionHeadingStyle vc |> toAttr, onClick action ]
            [ span [ collapsibleSectionIconStyle |> toAttr ] [ FontAwesome.icon icon |> Html.fromUnstyled ]
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
    , topLeftPanel vc gc model
    , graphToolsView plugins states vc gc model
    , topRightPanel plugins states vc gc model
    ]


topLeftPanel : View.Config -> Pathfinder.Config -> Model -> Html Msg
topLeftPanel vc gc model =
    div [ topLeftPanelStyle vc |> toAttr ]
        [ h2 [ vc.theme.heading2 |> toAttr ] [ Html.text "Pathfinder" ]
        , settingsView vc gc model
        ]


settingsView : View.Config -> Pathfinder.Config -> Model -> Html Msg
settingsView vc _ m =
    div [ searchViewStyle vc |> toAttr ]
        [ h3 [ panelHeadingStyle2 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Display") ]
        , case m.view.pointerTool of
            Drag ->
                Util.View.switch vc [ HA.checked True, onClick (ChangePointerTool Select |> ChangedDisplaySettingsMsg) ] "Drag"

            Select ->
                Util.View.switch vc [ HA.checked False, onClick (ChangePointerTool Drag |> ChangedDisplaySettingsMsg) ] "Select"
        ]


graphToolsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
graphToolsView _ _ vc _ _ =
    div
        [ graphToolsStyle vc |> toAttr
        ]
        (graphTools |> List.map (graphToolButton vc))


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
topRightPanel plugins ms vc gc model =
    div [ topRightPanelStyle vc |> toAttr ]
        [ graphActionsView vc gc model
        , searchBoxView plugins ms vc gc model
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
        [ h3 [ panelHeadingStyle2 vc |> toAttr ] [ Html.text (Locale.string vc.locale "Add to graph") ]
        , div [ searchBoxContainerStyle vc |> toAttr ]
            [ span [ searchBoxIconStyle vc |> toAttr ] [ FontAwesome.icon FontAwesome.search |> Html.fromUnstyled ]
            , View.Search.search plugins vc { css = searchInputStyle vc, multiline = False, resultsAsLink = True, showIcon = False } model.search |> Html.map SearchMsg
            ]
        ]


detailsView : View.Config -> Pathfinder.Config -> Model -> Html Msg
detailsView vc gc model =
    if isDetailsViewVisible model then
        div
            [ detailsViewStyle vc |> toAttr ]
            [ detailsViewCloseRow vc
            , case ( model.selection, getDetailsViewStateForSelection model ) of
                ( SelectedAddress id, AddressDetails _ state ) ->
                    getAddress model.network.addresses id
                        |> Result.map .data
                        |> Result.Extra.unpack
                            (Error.view vc)
                            (RemoteData.unwrap
                                (Util.View.loadingSpinner vc Css.View.loadingSpinner)
                                (addressDetailsContentView vc gc model id state)
                            )

                ( SelectedTx id, TxDetails _ state ) ->
                    Dict.get id model.network.txs
                        |> Maybe.map (\x -> txDetailsContentView vc gc model id state x.raw)
                        |> Maybe.withDefault (Util.View.loadingSpinner vc Css.View.loadingSpinner)

                _ ->
                    none
            ]

    else
        none


detailsViewCloseRow : View.Config -> Html Msg
detailsViewCloseRow vc =
    div [ detailsViewCloseButtonStyle |> toAttr ] [ closeButton vc UserClosedDetailsView ]


closeButton : View.Config -> Msg -> Html Msg
closeButton vc msg =
    button [ linkButtonStyle vc True |> toAttr, msg |> onClick ] [ FontAwesome.icon FontAwesome.times |> Html.fromUnstyled ]


getAddressAnnotationBtns : Api.Data.Address -> Maybe Api.Data.Actor -> List BtnConfig
getAddressAnnotationBtns data actor =
    let
        hasTags _ =
            not (actor == Nothing)

        isContract x =
            x.isContract |> Maybe.withDefault False
    in
    (if hasTags data then
        [ BtnConfig FontAwesome.tags "has tags" NoOp True ]

     else
        []
    )
        ++ (if isContract data then
                [ BtnConfig FontAwesome.cog "is contract" NoOp True ]

            else
                []
           )
        ++ (actor |> Maybe.map (\a -> [ BtnConfig FontAwesome.user a.label NoOp True ]) |> Maybe.withDefault [])


getAddressActionBtns : Api.Data.Address -> List BtnConfig
getAddressActionBtns data =
    [ BtnConfig FontAwesome.tags "Connect case" NoOp True, BtnConfig FontAwesome.cog "Actions" NoOp True ]


txDetailsContentView : View.Config -> Pathfinder.Config -> Model -> Id -> TxDetailsViewState -> Api.Data.Tx -> Html Msg
txDetailsContentView vc gc model id viewState data =
    let
        header =
            [ longIdentDetailsHeadingView vc gc id "Transaction" []
            , rule
            ]

        ( detailsTblBody, sections ) =
            case data of
                Api.Data.TxTxAccount tx ->
                    ( [ accountTxDetailsContentView vc tx ]
                    , [ none ]
                    )

                Api.Data.TxTxUtxo tx ->
                    ( [ utxoTxDetailsContentView vc tx ]
                    , [ utxoTxDetailsSectionsView vc viewState tx ]
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
            [ BtnConfig FontAwesome.tags "Do it" NoOp True ]

        tbls =
            [ detailsFactTableView vc (apiUtxoTxToRows data), detailsActionsView vc actionBtns ]
    in
    div [] tbls


ioTableView : View.Config -> String -> List Api.Data.TxValue -> Html Msg
ioTableView vc currency data =
    Table.rawTableView vc [] (IoTable.config vc currency) "Value" data


utxoTxDetailsSectionsView : View.Config -> TxDetailsViewState -> Api.Data.TxUtxo -> Html Msg
utxoTxDetailsSectionsView vc viewState data =
    let
        combinedData =
            (data.inputs |> Maybe.withDefault []) ++ (data.outputs |> Maybe.withDefault [] |> List.map negateTxValue)

        content =
            ioTableView vc data.currency combinedData

        ioIndicatorState =
            Just (inOutIndicator Nothing data.noInputs data.noOutputs)
    in
    collapsibleSection vc "In- and Outputs" viewState.ioTableOpen ioIndicatorState content (TxDetailsMsg UserClickedToggleIOTable)


accountTxDetailsContentView : View.Config -> Api.Data.TxAccount -> Html Msg
accountTxDetailsContentView vc data =
    div [] [ Html.text "I am a Account TX" ]


addressDetailsContentView : View.Config -> Pathfinder.Config -> Model -> Id -> AddressDetailsViewState -> Api.Data.Address -> Html Msg
addressDetailsContentView vc gc model id viewState data =
    let
        actor_id =
            data.actors |> Maybe.andThen (List.head >> Maybe.map .id)

        actor =
            actor_id |> Maybe.andThen (\i -> Dict.get i model.actors)

        addressImg =
            actor |> Maybe.andThen .context |> Maybe.andThen (.images >> List.head) |> Maybe.map addProtocolPrefx |> Maybe.withDefault (dummyImageSrc vc)

        actor_text =
            actor |> Maybe.map .label |> Maybe.withDefault ""

        txOnGraphFn =
            \txId -> Dict.member txId model.network.txs

        sections =
            [ addressTransactionTableView vc gc id viewState txOnGraphFn model data
            , addressNeighborsTableView vc gc id viewState data
            ]

        tbls =
            [ detailsFactTableView vc (apiAddressToRows data), detailsActionsView vc (getAddressActionBtns data) ]

        addressAnnotationBtns =
            getAddressAnnotationBtns data actor
    in
    div []
        (div [ detailsContainerStyle |> toAttr ]
            [ div [ detailsViewContainerStyle vc |> toAttr ]
                [ img [ src addressImg, HA.alt actor_text, HA.title actor_text, addressDetailsViewActorImageStyle vc |> toAttr ] []
                , div [ fullWidth |> toAttr ]
                    ([ longIdentDetailsHeadingView vc gc id "Address" addressAnnotationBtns
                     , rule
                     ]
                        ++ tbls
                    )
                ]
            ]
            :: sections
        )


addressTransactionTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> (Id -> Bool) -> Model -> Api.Data.Address -> Html Msg
addressTransactionTableView vc gc id viewState txOnGraphFn m data =
    let
        attributes =
            []

        prevMsg =
            \_ -> AddressDetailsMsg UserClickedPreviousPageTransactionTable

        nextMsg =
            \_ -> AddressDetailsMsg UserClickedNextPageTransactionTable

        content =
            div []
                (if DatePicker.isOpen m.dateRangePicker then
                    [ secondaryButton vc (BtnConfig FontAwesome.check "Ok" CloseDateRangePicker True)
                    , DatePicker.view (userDefinedRangeDatePickerSettings vc.locale m.currentTime) m.dateRangePicker |> Html.fromUnstyled
                    ]

                 else
                    let
                        startP =
                            m.fromDate |> Maybe.withDefault (Time.millisToPosix <| data.firstTx.timestamp * 1000)

                        endP =
                            m.toDate |> Maybe.withDefault (Time.millisToPosix <| data.lastTx.timestamp * 1000)

                        selectedDuration =
                            Locale.durationPosix vc.locale 1 startP endP

                        startS =
                            Locale.posixDate vc.locale startP

                        endS =
                            Locale.posixDate vc.locale endP
                    in
                    [ div []
                        [ div [ dateTimeRangeBoxStyle vc |> toAttr ]
                            [ FontAwesome.iconWithOptions FontAwesome.calendar FontAwesome.Regular [] [] |> Html.fromUnstyled
                            , span [] [ Html.text selectedDuration ]
                            , span [ dateTimeRangeHighlightedDateStyle vc |> toAttr ] [ Html.text startS ]
                            , span [] [ Html.text (Locale.string vc.locale "to") ]
                            , span [ dateTimeRangeHighlightedDateStyle vc |> toAttr ] [ Html.text endS ]
                            , span [ dateTimeRangeFilterButtonStyle vc |> toAttr ] [ secondaryButton vc (BtnConfig FontAwesome.filter "" OpenDateRangePicker True) ]
                            ]
                        , Table.pagedTableView vc attributes (TransactionTable.config vc data.currency txOnGraphFn) viewState.txs prevMsg nextMsg
                        ]
                    ]
                )

        ioIndicatorState =
            Just (inOutIndicator (Just (data.noIncomingTxs + data.noOutgoingTxs)) data.noIncomingTxs data.noOutgoingTxs)
    in
    collapsibleSection vc "Transactions" viewState.transactionsTableOpen ioIndicatorState content (AddressDetailsMsg UserClickedToggleTransactionTable)


addressNeighborsTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> Api.Data.Address -> Html Msg
addressNeighborsTableView vc gc id viewState data =
    let
        attributes =
            []

        prevMsg =
            \dir _ -> AddressDetailsMsg (UserClickedPreviousPageNeighborsTable dir)

        nextMsg =
            \dir _ -> AddressDetailsMsg (UserClickedNextPageNeighborsTable dir)

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
    collapsibleSection vc "Neighbors" viewState.neighborsTableOpen ioIndicatorState content (AddressDetailsMsg UserClickedToggleNeighborsTable)


longIdentDetailsHeadingView : View.Config -> Pathfinder.Config -> Id -> String -> List BtnConfig -> Html Msg
longIdentDetailsHeadingView vc gc id typeName annotations =
    let
        mNetwork =
            Id.network id

        heading =
            String.trim (String.join " " [ mNetwork, Locale.string vc.locale typeName ])
    in
    div []
        [ h1 [ panelHeadingStyle2 vc |> toAttr ] (Html.text (String.toUpper heading) :: (annotations |> List.map (annotationButton vc)))
        , copyableLongIdentifier vc [ copyableIdentifierStyle vc |> toAttr ] (Id.id id)
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
    ]


apiUtxoTxToRows : Api.Data.TxUtxo -> List KVTableRow
apiUtxoTxToRows tx =
    [ Row "Timstamp" (Timestamp tx.timestamp)
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
    Components.buttonMediumContainedPrimary [] []



{-
   let
       btnType i =
           if i == 0 then
               Primary

           else
               Secondary
   in
   div [ smPaddingBottom |> toAttr ] (actionButtons |> List.indexedMap (\i itm -> detailsActionButton vc (btnType i) itm))
-}


graphSvg : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> BBox -> Svg Msg
graphSvg plugins _ vc gc model bbox =
    let
        dim =
            { width = bbox.width, height = bbox.height }

        pointer =
            case ( model.dragging, model.view.pointerTool ) of
                ( Dragging _ _ _, Drag ) ->
                    Css.grabbing

                ( _, Select ) ->
                    Css.crosshair

                _ ->
                    Css.grab

        pointerStyle =
            [ Css.cursor pointer ]
    in
    svg
        ([ preserveAspectRatio "xMidYMid meet"
         , Transform.viewBox dim model.transform |> viewBox
         , (Css.Graph.svgRoot vc ++ pointerStyle) |> SA.css
         , UserClickedGraph model.dragging
            |> onClick
         , SA.id "graph"
         , Svg.custom "wheel"
            (Json.Decode.map3
                (\y mx my ->
                    { message = UserWheeledOnGraph mx my y
                    , stopPropagation = False
                    , preventDefault = False
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
        [ Svg.lazy4 Network.addresses plugins vc gc model.network.addresses
        , Svg.lazy4 Network.txs plugins vc gc model.network.txs
        , Svg.lazy5 Network.edges plugins vc gc model.network.addresses model.network.txs
        , drawDragSelector vc model

        -- , rect [ fill "black", width "1", height "1", x "0", y "0" ] [] -- Mark zero point in coordinate system
        ]


drawDragSelector : View.Config -> Model -> Svg Msg
drawDragSelector vc m =
    case ( m.dragging, m.view.pointerTool ) of
        ( Dragging tm start now, Select ) ->
            let
                crd =
                    case tm.state of
                        Settled c ->
                            c

                        Transitioning v ->
                            v.from

                z =
                    value crd.z

                xn =
                    Basics.min start.x now.x * z + crd.x

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