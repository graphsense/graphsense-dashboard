module View.Pathfinder exposing (view)

import Api.Data
import CIString
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Graph
import Css.Pathfinder as Css exposing (..)
import FontAwesome
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (disabled, id, src)
import Html.Styled.Lazy exposing (..)
import Json.Decode
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (BBox, Coords)
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as SA exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Util.Graph
import Util.View exposing (copyableLongIdentifier, none)
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.Network as Network
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

        Currency v _ ->
            span [] [ Html.text (String.fromInt v.value) ]

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


inIcon : Html Msg
inIcon =
    span [ inIconStyle |> toAttr ] [ FontAwesome.icon FontAwesome.signInAlt |> Html.fromUnstyled ]


outIcon : Html Msg
outIcon =
    span [ outIconStyle |> toAttr ] [ FontAwesome.icon FontAwesome.signOutAlt |> Html.fromUnstyled ]


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
    , contents = graph plugins states vc {} model
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
            , case model.view.detailsViewState of
                Address id config data ->
                    addressDetailsContentView plugins ms vc gc model id config data

                NoDetails ->
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


getAddressAnnotationBtns : Api.Data.Address -> List BtnConfig
getAddressAnnotationBtns data =
    let
        hasTags _ =
            True

        isContract x =
            not (x.isContract == Nothing)
    in
    (if hasTags data then
        [ BtnConfig FontAwesome.tags "tags" NoOp True ]

     else
        []
    )
        ++ (if isContract data then
                [ BtnConfig FontAwesome.cog "is contract" NoOp True ]

            else
                []
           )


getAddressActionBtns : Api.Data.Address -> List BtnConfig
getAddressActionBtns data =
    [ BtnConfig FontAwesome.tags "Connect case" NoOp True, BtnConfig FontAwesome.cog "Actions" NoOp True ]


addressDetailsContentView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Id -> AddressDetailsViewState -> Maybe Api.Data.Address -> Html Msg
addressDetailsContentView plugins ms vc gc model id viewState mdata =
    let
        addressImg =
            dummyImageSrc vc

        ( sections, tbls, addressAnnotationBtns ) =
            case mdata of
                Nothing ->
                    -- LOADING VIEW
                    ( [], [ span [] [ Html.text "Loading ..." ] ], [] )

                Just data ->
                    ( [ addressTransactionTableView vc gc id viewState data, addressNeighborsTableView vc gc id viewState data ]
                    , [ addressDetailsTableView vc gc id viewState data, addressActionsView vc gc id viewState data (getAddressActionBtns data) ]
                    , getAddressAnnotationBtns data
                    )
    in
    div []
        ([ div [ addressDetailsContainerStyle |> toAttr ]
            [ div [ detailsViewContainerStyle vc |> toAttr ]
                [ img [ src addressImg, addressDetailsViewActorImageStyle vc |> toAttr ] []
                , div [ fullWidth |> toAttr ]
                    ([ addressDetailsHeadingView vc gc id (mdata |> Maybe.map .currency) addressAnnotationBtns
                     , rule
                     ]
                        ++ tbls
                    )
                ]
            ]
         ]
            ++ sections
        )


addressTransactionTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> Api.Data.Address -> Html Msg
addressTransactionTableView vc gc id viewState data =
    let
        content =
            table [ fullWidth |> toAttr ]
                [ tr []
                    [ th [] [ Html.text "Timestamp" ]
                    , th [] [ Html.text "Hash" ]
                    , th [] [ Html.text "Debit/Credit" ]
                    ]
                , tr []
                    [ td [] [ Html.text "bla" ]
                    , td [] [ Html.text "bla" ]
                    , td [] [ Html.text "1" ]
                    ]
                , tr []
                    [ td [] [ Html.text "bla" ]
                    , td [] [ Html.text "bla" ]
                    , td [] [ Html.text "-1" ]
                    ]
                , tr []
                    [ td [] [ Html.text "bla" ]
                    , td [] [ Html.text "bla" ]
                    , td [] [ Html.text "2" ]
                    ]
                ]
    in
    collapsibleSection vc "Transactions" viewState.transactionsTableOpen (Just (inOutIndicator (Just (data.noIncomingTxs + data.noOutgoingTxs)) data.noIncomingTxs data.noOutgoingTxs)) content UserClickedToggleTransactionDetailsTable


addressNeighborsTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> Api.Data.Address -> Html Msg
addressNeighborsTableView vc gc id viewState data =
    let
        content =
            div [] [ Html.text "yeeeha" ]
    in
    collapsibleSection vc "Addresses" viewState.addressTableOpen (Just (inOutIndicator Nothing data.inDegree data.outDegree)) content UserClickedToggleAddressDetailsTable


addressDetailsHeadingView : View.Config -> Pathfinder.Config -> Id -> Maybe String -> List BtnConfig -> Html Msg
addressDetailsHeadingView vc gc id mNetwork annotations =
    let
        heading =
            String.trim (String.join " " [ mNetwork |> Maybe.withDefault "", Locale.string vc.locale "Address" ])
    in
    div []
        [ h1 [ panelHeadingStyle2 vc |> toAttr ] (Html.text (String.toUpper heading) :: (annotations |> List.map (annotationButton vc)))
        , copyableLongIdentifier vc [ copyableIdentifierStyle vc |> toAttr ] (Id.id id)
        ]


annotationButton : View.Config -> BtnConfig -> Html Msg
annotationButton vc btn =
    disableableButton (linkButtonStyle vc) btn [] [ FontAwesome.icon btn.icon |> Html.fromUnstyled ]


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
    in
    svg
        ([ preserveAspectRatio "xMidYMid meet"
         , Transform.viewBox dim model.transform |> viewBox
         , Css.Graph.svgRoot vc |> SA.css
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
        [ model.network |> Svg.lazy4 networks plugins vc gc
        ]


networks : Plugins -> View.Config -> Pathfinder.Config -> Network -> Svg Msg
networks plugins vc gc network =
    Svg.lazy4 Network.addresses plugins vc gc network.addresses