module View.Pathfinder exposing (view)

import Api.Data
import Browser.Events exposing (Visibility(..))
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Graph
import Css.Pathfinder as Css exposing (..)
import Css.View
import Dict
import FontAwesome
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
import Model.Pathfinder.Id as Id exposing (Id)
import Msg.Pathfinder exposing (AddressDetailsMsg(..), Msg(..))
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
    = Value Int
    | Currency Api.Data.Values String
    | Timestamp Int


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
                , td [ kVTableValueTdStyle vc |> toAttr ] (renderKVTableValue vc value |> List.singleton)
                , td [ kVTableTdStyle vc |> toAttr ] (renderKVTableValueExtension vc value |> List.singleton)
                ]

        Gap ->
            tr []
                [ td [ kVTableKeyTdStyle vc |> toAttr ] []
                , td [] []
                , td [] []
                ]


renderKVTableValue : View.Config -> ValueType -> Html Msg
renderKVTableValue vc val =
    case val of
        Value v ->
            span [] [ Html.text (String.fromInt v) ]

        Currency v ticker ->
            span [ HA.title (String.fromInt v.value) ] [ Html.text (Locale.coinWithoutCode vc.locale (assetFromBase ticker) v.value) ]

        Timestamp ts ->
            span [] [ Locale.timestampDateUniform vc.locale ts |> Html.text ]


renderKVTableValueExtension : View.Config -> ValueType -> Html Msg
renderKVTableValueExtension _ val =
    case val of
        Value _ ->
            none

        Currency _ ticker ->
            span [] [ Html.text (String.toUpper ticker) ]

        Timestamp _ ->
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
    , topLeftPanel plugins states vc gc model
    , graphToolsView plugins states vc gc model
    , topRightPanel plugins states vc gc model
    ]


topLeftPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
topLeftPanel _ _ vc _ _ =
    div [ topLeftPanelStyle vc |> toAttr ]
        [ h2 [ vc.theme.heading2 |> toAttr ] [ Html.text "Pathfinder" ]

        --, settingsView plugins ms vc gc model
        ]


settingsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
settingsView _ _ vc _ _ =
    div [ searchViewStyle vc |> toAttr ] [ Html.text "Display", FontAwesome.icon FontAwesome.chevronDown |> Html.fromUnstyled ]


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
        , detailsView plugins ms vc gc model
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


detailsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
detailsView plugins ms vc gc model =
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
                                (addressDetailsContentView plugins ms vc gc model id state)
                            )

                ( SelectedTx id, TxDetails ) ->
                    Dict.get id model.network.txs
                        |> Maybe.map (\x -> txDetailsContentView plugins ms vc gc model id x.raw)
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


txDetailsContentView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Id -> Api.Data.Tx -> Html Msg
txDetailsContentView plugins ms vc gc model id data =
    let
        content =
            case data of
                Api.Data.TxTxAccount tx ->
                    [ longIdentDetailsHeadingView vc gc id (Just tx.currency) "Transaction" []
                    , rule
                    , accountTxDetailsContentView vc tx
                    ]

                Api.Data.TxTxUtxo tx ->
                    [ longIdentDetailsHeadingView vc gc id (Just tx.currency) "Transaction" []
                    , rule
                    , utxoTxDetailsContentView vc tx
                    ]
    in
    div [ detailsContainerStyle |> toAttr ]
        [ div [ detailsViewContainerStyle vc |> toAttr ]
            [ div [ fullWidth |> toAttr ] content
            ]
        ]


utxoTxDetailsContentView : View.Config -> Api.Data.TxUtxo -> Html Msg
utxoTxDetailsContentView vc data =
    div [] [ Html.text "I am a UTXO TX" ]


accountTxDetailsContentView : View.Config -> Api.Data.TxAccount -> Html Msg
accountTxDetailsContentView vc data =
    div [] [ Html.text "I am a Account TX" ]


addressDetailsContentView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Id -> AddressDetailsViewState -> Api.Data.Address -> Html Msg
addressDetailsContentView plugins ms vc gc model id viewState data =
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
            [ addressTransactionTableView vc gc id viewState txOnGraphFn data
            , addressNeighborsTableView vc gc id viewState data
            ]

        tbls =
            [ addressDetailsTableView vc gc id viewState data, addressActionsView vc gc id viewState data (getAddressActionBtns data) ]

        addressAnnotationBtns =
            getAddressAnnotationBtns data actor
    in
    div []
        (div [ detailsContainerStyle |> toAttr ]
            [ div [ detailsViewContainerStyle vc |> toAttr ]
                [ img [ src addressImg, HA.alt actor_text, HA.title actor_text, addressDetailsViewActorImageStyle vc |> toAttr ] []
                , div [ fullWidth |> toAttr ]
                    ([ longIdentDetailsHeadingView vc gc id (Just data.currency) "Address" addressAnnotationBtns
                     , rule
                     ]
                        ++ tbls
                    )
                ]
            ]
            :: sections
        )


addressTransactionTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> (Id -> Bool) -> Api.Data.Address -> Html Msg
addressTransactionTableView vc gc id viewState txOnGraphFn data =
    let
        toolsConfig =
            { filter = Nothing, csv = Nothing }

        attributes =
            []

        prevMsg =
            \_ -> AddressDetailsMsg UserClickedPreviousPageTransactionTable

        nextMsg =
            \_ -> AddressDetailsMsg UserClickedNextPageTransactionTable

        content =
            Table.pagedTableView vc attributes toolsConfig (TransactionTable.config vc data.currency txOnGraphFn) viewState.txs prevMsg nextMsg
    in
    collapsibleSection vc "Transactions" viewState.transactionsTableOpen (Just (inOutIndicator (Just (data.noIncomingTxs + data.noOutgoingTxs)) data.noIncomingTxs data.noOutgoingTxs)) content (AddressDetailsMsg UserClickedToggleTransactionTable)


addressNeighborsTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> Api.Data.Address -> Html Msg
addressNeighborsTableView vc gc id viewState data =
    let
        toolsConfig =
            { filter = Nothing, csv = Nothing }

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
                , Table.pagedTableView vc attributes toolsConfig tblCfg viewState.neighborsOutgoing (prevMsg Outgoing) (nextMsg Outgoing)
                , h2 [ panelHeadingStyle2 vc |> toAttr ] [ Html.text "Incoming" ]
                , Table.pagedTableView vc attributes toolsConfig tblCfg viewState.neighborsIncoming (prevMsg Incoming) (nextMsg Incoming)
                ]
    in
    collapsibleSection vc "Neighbors" viewState.neighborsTableOpen (Just (inOutIndicator Nothing data.inDegree data.outDegree)) content (AddressDetailsMsg UserClickedToggleNeighborsTable)


longIdentDetailsHeadingView : View.Config -> Pathfinder.Config -> Id -> Maybe String -> String -> List BtnConfig -> Html Msg
longIdentDetailsHeadingView vc gc id mNetwork typeName annotations =
    let
        heading =
            String.trim (String.join " " [ mNetwork |> Maybe.withDefault "", Locale.string vc.locale typeName ])
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


addressDetailsTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> Api.Data.Address -> Html Msg
addressDetailsTableView vc gc id viewState addressData =
    div [ smPaddingBottom |> toAttr ] [ renderKVTable vc (apiAddressToRows addressData) ]


detailsActionButton : View.Config -> ButtonType -> BtnConfig -> Html Msg
detailsActionButton vc btnT btn =
    disableableButton (detailsActionButtonStyle vc btnT) btn [] [ Html.text (Locale.string vc.locale btn.text) ]


addressActionsView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> Api.Data.Address -> List BtnConfig -> Html Msg
addressActionsView vc gc id viewState addressData actionButtons =
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
