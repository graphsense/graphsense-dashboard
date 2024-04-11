module View.Pathfinder exposing (view)

import Api.Data
import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Graph
import Css.Pathfinder as Css
import FontAwesome
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (..)
import Html.Styled.Lazy exposing (..)
import Json.Decode
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (BBox, Coords)
import Model.Pathfinder exposing (..)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import ProgramTest exposing (within)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Keyed as Keyed
import Svg.Styled.Lazy as Svg
import Util.Graph
import Util.View exposing (copyableLongIdentifier, none)
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.Network as Network
import View.Search



-- Styles
-- http://probablyprogramming.com/2009/03/15/the-tiniest-gif-ever


type ButtonType
    = Primary
    | Secondary


dummyImageSrc : View.Config -> String
dummyImageSrc _ =
    "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs="


highlightPrimaryColor : Css.Color
highlightPrimaryColor =
    Css.rgb 26 197 176


highlightPrimaryColorFrosted : Css.Color
highlightPrimaryColorFrosted =
    Css.rgb 107 203 186


lighterGreyColor : Css.Color
lighterGreyColor =
    Css.rgb 208 216 220


lightGreyColor : Css.Color
lightGreyColor =
    Css.rgb 120 144 156


defaultBackgroundColor : Css.Color
defaultBackgroundColor =
    Css.rgb 255 255 255


blackColor : Css.Color
blackColor =
    Css.rgb 0 0 0


whiteColor : Css.Color
whiteColor =
    Css.rgb 255 255 255


greenColor : Css.Color
greenColor =
    Css.rgb 141 194 153


redColor : Css.Color
redColor =
    Css.rgb 194 141 141


boxStyle : View.Config -> Maybe Float -> List Css.Style
boxStyle _ padding =
    [ Css.backgroundColor defaultBackgroundColor
    , Css.boxShadow5 (Css.px 1) (Css.px 1) (Css.px 5) (Css.px 1) lighterGreyColor
    , Css.px (padding |> Maybe.withDefault 10) |> Css.padding
    ]


searchInputStyle : View.Config -> String -> List Css.Style
searchInputStyle _ _ =
    [ Css.pct 100 |> Css.width
    , Css.px 20 |> Css.height
    , Css.display Css.block
    , Css.color lightGreyColor
    , Css.border3 (Css.px 1) Css.solid lighterGreyColor
    , Css.backgroundColor defaultBackgroundColor
    , Css.px 3 |> Css.borderRadius
    , Css.px 25 |> Css.textIndent
    ]


panelHeadingStyle : View.Config -> List Css.Style
panelHeadingStyle _ =
    [ Css.fontWeight Css.bold
    , Css.em 1.4 |> Css.fontSize
    , Css.px 10 |> Css.marginBottom
    ]


panelHeadingStyle2 : View.Config -> List Css.Style
panelHeadingStyle2 _ =
    [ Css.fontWeight Css.bold
    , Css.em 1.3 |> Css.fontSize
    , Css.px 10 |> Css.marginBottom
    ]


collapsibleSectionHeadingStyle : View.Config -> List Css.Style
collapsibleSectionHeadingStyle _ =
    [ Css.fontWeight Css.bold
    , Css.em 1.1 |> Css.fontSize
    , Css.px 10 |> Css.marginBottom
    , Css.px 10 |> Css.marginTop
    , Css.borderBottom3 (Css.px 0.3) Css.solid lighterGreyColor
    , Css.px 30 |> Css.height
    , Css.cursor Css.pointer
    ]


toolItemStyle : View.Config -> List Css.Style
toolItemStyle _ =
    [ Css.px 55 |> Css.minWidth
    , Css.textAlign Css.center
    ]


linkButtonStyle : View.Config -> Bool -> List Css.Style
linkButtonStyle _ enabled =
    [ Css.backgroundColor defaultBackgroundColor, Css.px 0 |> Css.borderWidth, Css.cursor Css.pointer, Css.px 0 |> Css.padding, Css.px 5 |> Css.paddingLeft ]


toolButtonStyle : View.Config -> Bool -> List Css.Style
toolButtonStyle vc enabled =
    Css.textAlign Css.center :: linkButtonStyle vc enabled


toolIconStyle : View.Config -> List Css.Style
toolIconStyle _ =
    [ Css.em 1.3 |> Css.fontSize
    , Css.px 5 |> Css.marginBottom
    ]


topLeftPanelStyle : View.Config -> List Css.Style
topLeftPanelStyle _ =
    [ Css.position Css.absolute
    , Css.px 10 |> Css.left
    , Css.px 10 |> Css.top
    ]


graphToolsStyle : View.Config -> List Css.Style
graphToolsStyle vc =
    [ Css.position Css.absolute
    , Css.pct 50 |> Css.left
    , Css.px 50 |> Css.bottom
    , Css.displayFlex
    , Css.transform (Css.translate (Css.pct -50))
    ]
        ++ boxStyle vc Nothing


topRightPanelStyle : View.Config -> List Css.Style
topRightPanelStyle _ =
    [ Css.position Css.absolute
    , Css.px 10 |> Css.right
    , Css.px 10 |> Css.top
    ]


searchBoxStyle : View.Config -> Maybe Float -> List Css.Style
searchBoxStyle vc padding =
    [ Css.px 300 |> Css.minWidth
    , Css.px 10 |> Css.marginBottom
    ]
        ++ boxStyle vc padding


detailsViewStyle : View.Config -> List Css.Style
detailsViewStyle vc =
    searchBoxStyle vc (Just 0)


graphActionsViewStyle : View.Config -> List Css.Style
graphActionsViewStyle _ =
    [ Css.displayFlex, Css.justifyContent Css.flexEnd, Css.px 5 |> Css.margin ]


graphActionButtonStyle : View.Config -> Bool -> List Css.Style
graphActionButtonStyle _ _ =
    [ Css.px 5 |> Css.margin
    , Css.cursor Css.pointer
    , Css.padding4 (Css.px 3) (Css.px 10) (Css.px 3) (Css.px 10)
    , Css.color lightGreyColor
    , Css.backgroundColor defaultBackgroundColor
    , Css.border3 (Css.px 1) Css.solid lighterGreyColor
    , Css.px 3 |> Css.borderRadius
    ]


detailsActionButtonStyle : View.Config -> ButtonType -> Bool -> List Css.Style
detailsActionButtonStyle _ bt _ =
    case bt of
        Primary ->
            [ Css.px 5 |> Css.margin
            , Css.cursor Css.pointer
            , Css.padding4 (Css.px 4) (Css.px 10) (Css.px 4) (Css.px 10)
            , Css.color whiteColor
            , Css.fontWeight Css.bold
            , Css.backgroundColor highlightPrimaryColorFrosted
            , Css.border3 (Css.px 1) Css.solid highlightPrimaryColorFrosted
            , Css.px 3 |> Css.borderRadius
            ]

        Secondary ->
            [ Css.px 5 |> Css.margin
            , Css.cursor Css.pointer
            , Css.padding4 (Css.px 4) (Css.px 10) (Css.px 4) (Css.px 10)
            , Css.color highlightPrimaryColorFrosted
            , Css.fontWeight Css.bold
            , Css.backgroundColor whiteColor
            , Css.border3 (Css.px 1) Css.solid highlightPrimaryColorFrosted
            , Css.px 3 |> Css.borderRadius
            ]


searchViewStyle : View.Config -> List Css.Style
searchViewStyle vc =
    boxStyle vc Nothing ++ [ Css.displayFlex, Css.justifyContent Css.flexEnd ]


searchBoxContainerStyle : View.Config -> List Css.Style
searchBoxContainerStyle _ =
    [ Css.position Css.relative ]


searchBoxIconStyle : View.Config -> List Css.Style
searchBoxIconStyle _ =
    [ Css.position Css.absolute, Css.px 7 |> Css.top, Css.px 7 |> Css.left ]


addressDetailsViewActorImageStyle : View.Config -> List Css.Style
addressDetailsViewActorImageStyle _ =
    [ Css.display Css.block
    , Css.borderRadius (Css.pct 50)
    , Css.height (Css.px 40)
    , Css.width (Css.px 40)
    , Css.border3 (Css.px 1) Css.solid blackColor
    , Css.px 5 |> Css.marginRight
    ]


detailsViewContainerStyle : View.Config -> List Css.Style
detailsViewContainerStyle _ =
    [ Css.displayFlex, Css.justifyContent Css.left, Css.pct 100 |> Css.width ]


kVTableTdStyle : View.Config -> List Css.Style
kVTableTdStyle _ =
    [ Css.px 5 |> Css.paddingLeft ]


kVTableKeyTdStyle : View.Config -> List Css.Style
kVTableKeyTdStyle vc =
    [ Css.px 5 |> Css.paddingTop, Css.px 5 |> Css.paddingBottom, Css.color lightGreyColor ] ++ kVTableTdStyle vc


kVTableValueTdStyle : View.Config -> List Css.Style
kVTableValueTdStyle vc =
    Css.textAlign Css.right :: kVTableTdStyle vc



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


renderKVTable : View.Config -> List KVTableRow -> Html Msg
renderKVTable vc rows =
    table [ [ Css.pct 100 |> Css.width ] |> HA.css ] (rows |> List.map (renderKVRow vc))


renderKVRow : View.Config -> KVTableRow -> Html Msg
renderKVRow vc row =
    case row of
        Row key value ->
            tr []
                [ td [ kVTableKeyTdStyle vc |> HA.css ] [ Html.text (Locale.string vc.locale key) ]
                , td [ kVTableValueTdStyle vc |> HA.css ] (renderKVTableValue vc value |> List.singleton)
                , td [ kVTableTdStyle vc |> HA.css ] (renderKVTableValueExtension vc value |> List.singleton)
                ]

        Gap ->
            tr []
                [ td [ kVTableKeyTdStyle vc |> HA.css ] []
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
    button (((style btn.enable |> HA.css) :: addattr) ++ attrs) content


rule : Html Msg
rule =
    hr [ [ Css.px 5 |> Css.marginBottom, Css.px 5 |> Css.marginTop ] |> HA.css ] []


inIcon : Html Msg
inIcon =
    span [ [ Css.color greenColor, Css.ch 0.5 |> Css.paddingRight, Css.ch 0.2 |> Css.paddingLeft ] |> HA.css ] [ FontAwesome.icon FontAwesome.signInAlt |> Html.fromUnstyled ]


outIcon : Html Msg
outIcon =
    span [ [ Css.color redColor, Css.ch 0.5 |> Css.paddingRight, Css.ch 0.2 |> Css.paddingLeft ] |> HA.css ] [ FontAwesome.icon FontAwesome.signOutAlt |> Html.fromUnstyled ]


inOutIndicator : Maybe Int -> Int -> Int -> Html Msg
inOutIndicator mnr inNr outNr =
    let
        prefix =
            case mnr of
                Just nr ->
                    String.join " " [ String.fromInt nr, "(" ]

                Nothing ->
                    "("
    in
    span [ [ Css.ch 0.5 |> Css.paddingLeft ] |> HA.css ] [ Html.text prefix, inIcon, Html.text (String.fromInt inNr), Html.text ",", outIcon, Html.text (String.fromInt outNr), Html.text ")" ]


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
        (div [ collapsibleSectionHeadingStyle vc |> HA.css, onClick action ]
            [ span [ [ Css.ch 1 |> Css.paddingRight, Css.ch 2 |> Css.paddingLeft ] |> HA.css ] [ FontAwesome.icon icon |> Html.fromUnstyled ]
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
    div [ topLeftPanelStyle vc |> HA.css ]
        [ h2 [ vc.theme.heading2 |> HA.css ] [ Html.text "Pathfinder" ]

        --, settingsView plugins ms vc gc model
        ]


settingsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
settingsView _ _ vc _ _ =
    div [ searchViewStyle vc |> HA.css ] [ Html.text "Display", FontAwesome.icon FontAwesome.chevronDown |> Html.fromUnstyled ]


graphToolsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
graphToolsView _ _ vc _ _ =
    div
        [ graphToolsStyle vc |> HA.css
        ]
        (graphTools |> List.map (graphToolButton vc))


graphToolButton : View.Config -> BtnConfig -> Svg Msg
graphToolButton vc btn =
    div [ toolItemStyle vc |> HA.css ]
        [ disableableButton (toolButtonStyle vc)
            btn
            []
            [ div [ toolIconStyle vc |> HA.css ] [ FontAwesome.icon btn.icon |> Html.fromUnstyled ]
            , Html.text (Locale.string vc.locale btn.text)
            ]
        ]


topRightPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
topRightPanel plugins ms vc gc model =
    div [ topRightPanelStyle vc |> HA.css ]
        [ graphActionsView vc gc model
        , searchBoxView plugins ms vc gc model
        , detailsView plugins ms vc gc model
        ]


graphActionsView : View.Config -> Pathfinder.Config -> Model -> Html Msg
graphActionsView vc _ _ =
    div [ graphActionsViewStyle vc |> HA.css ]
        (graphActionButtons |> List.map (graphActionButton vc))


graphActionButton : View.Config -> BtnConfig -> Html Msg
graphActionButton vc btn =
    disableableButton (graphActionButtonStyle vc) btn [] (iconWithText vc btn.icon (Locale.string vc.locale btn.text))


iconWithText : View.Config -> FontAwesome.Icon -> String -> List (Html Msg)
iconWithText _ faIcon text =
    [ span [ [ Css.px 5 |> Css.paddingRight ] |> HA.css ] [ FontAwesome.icon faIcon |> Html.fromUnstyled ], Html.text text ]


searchBoxView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
searchBoxView plugins _ vc _ model =
    div
        [ searchBoxStyle vc Nothing |> HA.css ]
        [ h3 [ panelHeadingStyle2 vc |> HA.css ] [ Html.text (Locale.string vc.locale "Add to graph") ]
        , div [ searchBoxContainerStyle vc |> HA.css ]
            [ span [ searchBoxIconStyle vc |> HA.css ] [ FontAwesome.icon FontAwesome.search |> Html.fromUnstyled ]
            , View.Search.search plugins vc { css = searchInputStyle vc, multiline = False, resultsAsLink = True, showIcon = False } model.search |> Html.map SearchMsg
            ]
        ]


detailsView : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> Html Msg
detailsView plugins ms vc gc model =
    if isDetailsViewVisible model then
        div
            [ detailsViewStyle vc |> HA.css ]
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
    div [ [ Css.float Css.right, Css.margin4 (Css.px 10) (Css.px 10) (Css.px 0) (Css.px 0) ] |> HA.css ] [ closeButton vc UserClosedDetailsView ]


closeButton : View.Config -> Msg -> Html Msg
closeButton vc msg =
    button [ linkButtonStyle vc True |> HA.css, msg |> onClick ] [ FontAwesome.icon FontAwesome.times |> Html.fromUnstyled ]


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
                    ( [], [ span [] [ Html.text "Loading ..." ] ], [] )

                Just data ->
                    ( [ transactionTableView vc gc id viewState data, addressNeighborsTableView vc gc id viewState data ]
                    , [ itemDetailsView vc gc id viewState data, itemActionsView vc gc id viewState data (getAddressActionBtns data) ]
                    , getAddressAnnotationBtns data
                    )
    in
    div []
        ([ div [ [ Css.px 10 |> Css.marginRight, Css.px 10 |> Css.marginLeft ] |> HA.css ]
            [ div [ detailsViewContainerStyle vc |> HA.css ]
                [ img [ src addressImg, addressDetailsViewActorImageStyle vc |> HA.css ] []
                , div [ [ Css.pct 100 |> Css.width ] |> HA.css ]
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


transactionTableView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> Api.Data.Address -> Html Msg
transactionTableView vc gc id viewState data =
    let
        content =
            table [ [ Css.pct 100 |> Css.width ] |> HA.css ]
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
        [ h1 [ panelHeadingStyle2 vc |> HA.css ] (Html.text (String.toUpper heading) :: (annotations |> List.map (annotationButton vc)))
        , copyableLongIdentifier vc [ [ Css.color highlightPrimaryColor ] |> HA.css ] id
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


itemDetailsView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> Api.Data.Address -> Html Msg
itemDetailsView vc gc id viewState addressData =
    div [ [ Css.paddingBottom (Css.px 10) ] |> HA.css ] [ renderKVTable vc (apiAddressToRows addressData) ]


detailsActionButton : View.Config -> ButtonType -> BtnConfig -> Html Msg
detailsActionButton vc btnT btn =
    disableableButton (detailsActionButtonStyle vc btnT) btn [] [ Html.text (Locale.string vc.locale btn.text) ]


itemActionsView : View.Config -> Pathfinder.Config -> Id -> AddressDetailsViewState -> Api.Data.Address -> List BtnConfig -> Html Msg
itemActionsView vc gc id viewState addressData actionButtons =
    let
        btnType i =
            if i == 0 then
                Primary

            else
                Secondary
    in
    div [ [ Css.paddingBottom (Css.px 10) ] |> HA.css ] (actionButtons |> List.indexedMap (\i itm -> detailsActionButton vc (btnType i) itm))


graphSvg : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Model -> BBox -> Svg Msg
graphSvg plugins _ vc gc model bbox =
    let
        dim =
            { width = bbox.width, height = bbox.height }
    in
    svg
        ([ preserveAspectRatio "xMidYMid meet"
         , Transform.viewBox dim model.transform |> viewBox
         , Css.Graph.svgRoot vc |> Svg.css
         , UserClickedGraph model.dragging
            |> onClick
         , HA.id "graph"
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
        [ model.network |> Maybe.map (Svg.lazy4 networks plugins vc gc) |> Maybe.withDefault (div [] [])
        ]


networks : Plugins -> View.Config -> Pathfinder.Config -> Network -> Svg Msg
networks plugins vc gc network =
    Keyed.node "g"
        []
        [ ( "Pathfinder.network." ++ network.name
          , Svg.lazy4 Network.addresses plugins vc gc network.addresses
          )
        ]
