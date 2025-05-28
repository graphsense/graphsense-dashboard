module View.Pathfinder exposing (view)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Graph
import Css.Pathfinder as Css
import Css.View
import Dict
import Hovercard
import Html.Styled as Html exposing (Html, div, input)
import Html.Styled.Attributes as HA
import Html.Styled.Events exposing (onClick, onInput, preventDefaultOn, stopPropagationOn)
import Json.Decode
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords as Coords exposing (BBox, Coords)
import Model.Graph.Transform exposing (Transition(..))
import Model.Locale as Locale
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.ContextMenu as ContextMenu exposing (ContextMenu)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tools exposing (PointerTool(..), ToolbarHovercardModel, ToolbarHovercardType(..))
import Msg.Pathfinder exposing (DisplaySettingsMsg(..), Msg(..))
import Number.Bounded exposing (value)
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import RemoteData
import String.Format
import Svg.Styled exposing (Svg, defs, linearGradient, stop, svg)
import Svg.Styled.Attributes exposing (css, height, id, offset, preserveAspectRatio, stopColor, transform, viewBox, width)
import Svg.Styled.Events as Svg
import Svg.Styled.Lazy as Svg
import Theme.Colors as Colors
import Theme.Html.GraphComponents as HGraphComponents
import Theme.Html.Icons as HIcons
import Theme.Html.SelectionControls exposing (SwitchSize(..))
import Theme.Html.SettingsComponents as Sc
import Theme.Svg.GraphComponents as GraphComponents
import Update.Graph.Transform as Transform
import Util.Annotations as Annotations
import Util.Css as Css
import Util.ExternalLinks
import Util.Graph
import Util.View exposing (hovercard, none)
import View.Controls as Vc
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.AddressDetails as AddressDetails
import View.Pathfinder.ContextMenuItem as ContextMenuItem
import View.Pathfinder.Network as Network
import View.Pathfinder.Toolbar as Toolbar
import View.Pathfinder.TxDetails as TxDetails
import View.Search



-- Helpers
-- View


view : Plugins -> ModelState -> View.Config -> Pathfinder.Model -> { navbar : List (Html Msg), contents : List (Html Msg) }
view plugins states vc model =
    { navbar = []
    , contents = graph plugins states vc model.config model
    }


graph : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> List (Html Msg)
graph plugins pluginStates vc gc model =
    [ vc.size
        |> Maybe.map (graphSvg plugins vc gc model)
        |> Maybe.withDefault none
    , topLeftPanel plugins pluginStates vc
    , topCenterPanel plugins pluginStates vc gc model
    , topRightPanel plugins pluginStates vc model
    ]
        ++ (model.toolbarHovercard
                |> Maybe.map (toolbarHovercardView vc model)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
           )
        ++ (model.contextMenu
                |> Maybe.map (contextMenuView plugins pluginStates vc model)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
           )


contextMenuView : Plugins -> ModelState -> View.Config -> Pathfinder.Model -> ContextMenu -> Html Msg
contextMenuView plugins pluginStates vc model ( coords, menu ) =
    let
        contextMenuWidth =
            180

        xposend =
            coords.x + contextMenuWidth

        xpos =
            if xposend > (vc.size |> Maybe.map .width |> Maybe.withDefault 0) then
                coords.x - contextMenuWidth - 80

            else
                coords.x - (contextMenuWidth / 2)
    in
    div
        [ [ Css.top (Css.px coords.y)
          , Css.left (Css.px xpos)
          , Css.position Css.absolute
          , Css.zIndex (Css.int 100)
          ]
            |> css
        , onClick UserClosesContextMenu
        ]
        [ case menu of
            ContextMenu.AddressIdChevronActions id ->
                HGraphComponents.rightClickMenuWithAttributes
                    (HGraphComponents.rightClickMenuAttributes
                        |> Rs.s_pluginsList [ [ Css.width (Css.pct 100) ] |> css ]
                        |> Rs.s_shortcutList [ [ Css.width (Css.pct 100) ] |> css ]
                        |> Rs.s_dividerLine [ [ Css.display Css.none ] |> css ]
                    )
                    { shortcutList =
                        Util.ExternalLinks.getBlockExplorerLinks (Id.network id) (Id.id id)
                            |> List.map
                                (\( url, name ) ->
                                    { icon = HIcons.iconsGoToS {}
                                    , text1 = name
                                    , text2 = Nothing
                                    , link = url
                                    , blank = True
                                    }
                                        |> ContextMenuItem.initLink2
                                        |> ContextMenuItem.view vc
                                )
                    , pluginsList = []
                    }
                    {}

            ContextMenu.TransactionIdChevronActions id ->
                HGraphComponents.rightClickMenuWithAttributes
                    (HGraphComponents.rightClickMenuAttributes
                        |> Rs.s_pluginsList [ [ Css.width (Css.pct 100) ] |> css ]
                        |> Rs.s_shortcutList [ [ Css.width (Css.pct 100) ] |> css ]
                        |> Rs.s_dividerLine [ [ Css.display Css.none ] |> css ]
                    )
                    { shortcutList =
                        Util.ExternalLinks.getBlockExplorerTransactionLinks (Id.network id) (Id.id id)
                            |> List.map
                                (\( url, name ) ->
                                    { icon = HIcons.iconsGoToS {}
                                    , text1 = name
                                    , text2 = Nothing
                                    , link = url
                                    , blank = True
                                    }
                                        |> ContextMenuItem.initLink2
                                        |> ContextMenuItem.view vc
                                )
                    , pluginsList = []
                    }
                    {}

            ContextMenu.AddressContextMenu id ->
                let
                    pluginsList =
                        Dict.get id model.network.addresses
                            |> Maybe.map
                                (Plugin.addressContextMenuNew plugins pluginStates vc
                                    >> List.map (ContextMenuItem.view vc)
                                )
                            |> Maybe.withDefault []
                in
                HGraphComponents.rightClickMenuWithAttributes
                    (HGraphComponents.rightClickMenuAttributes
                        |> Rs.s_pluginsList [ [ Css.width (Css.pct 100) ] |> css ]
                        |> Rs.s_shortcutList [ [ Css.width (Css.pct 100) ] |> css ]
                        |> (if List.isEmpty pluginsList then
                                Rs.s_dividerLine [ [ Css.display Css.none ] |> css ]

                            else
                                identity
                           )
                    )
                    { shortcutList =
                        [ { msg = UserOpensAddressAnnotationDialog id
                          , icon = HIcons.iconsAnnotateS {}
                          , text = "Annotate address"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuIdToClipboard menu
                          , icon = HIcons.iconsCopyS {}
                          , text = "Copy address ID"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuDeleteIcon menu
                          , icon = HIcons.iconsDeleteS {}
                          , text = "Remove from Graph"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuOpenInNewTab menu
                          , icon = HIcons.iconsGoToS {}
                          , text = "Open in new tab"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.view vc
                        ]
                    , pluginsList = pluginsList
                    }
                    {}

            ContextMenu.TransactionContextMenu _ ->
                HGraphComponents.rightClickMenuWithAttributes
                    (HGraphComponents.rightClickMenuAttributes
                        |> Rs.s_pluginsList [ [ Css.width (Css.pct 100) ] |> css ]
                        |> Rs.s_shortcutList [ [ Css.width (Css.pct 100) ] |> css ]
                    )
                    { shortcutList =
                        [ { msg = UserClickedContextMenuIdToClipboard menu
                          , icon = HIcons.iconsCopyS {}
                          , text = "Copy transaction ID"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuDeleteIcon menu
                          , icon = HIcons.iconsDeleteS {}
                          , text = "Remove from Graph"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuOpenInNewTab menu
                          , icon = HIcons.iconsGoToS {}
                          , text = "Open in new tab"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.view vc
                        ]
                    , pluginsList = []
                    }
                    {}
        ]


topCenterPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> Html Msg
topCenterPanel plugins _ vc gc model =
    div
        [ css Css.topPanelStyle
        ]
        [ div [] []
        , div
            [ css
                [ Css.displayFlex
                , Css.property "gap" "10px"
                , Css.property "pointer-events" "all"
                ]
            ]
            [ searchBoxView plugins vc gc model
            , Toolbar.view vc
                { undoDisabled = List.isEmpty model.history.past
                , redoDisabled = List.isEmpty model.history.future
                , deleteDisabled = model.selection == Pathfinder.NoSelection
                , newDisabled = not model.isDirty
                , annotateDisabled =
                    case model.selection of
                        Pathfinder.SelectedAddress _ ->
                            False

                        Pathfinder.SelectedTx _ ->
                            False

                        _ ->
                            True
                , pointerTool = model.pointerTool
                , exportName = model.name
                }
            ]
        , div
            [ css [ Css.property "pointer-events" "all" ] ]
            [ graphActionsView vc gc model
            ]
        ]


topLeftPanel : Plugins -> ModelState -> View.Config -> Html Msg
topLeftPanel plugins pluginStates vc =
    div [ Css.topLeftPanelStyle vc |> css ]
        (Plugin.pathfinderUpperLeftPanel plugins pluginStates vc)


toolbarHovercardView : View.Config -> Pathfinder.Model -> ToolbarHovercardModel -> Html Msg
toolbarHovercardView vc m ( hcid, hc ) =
    case ( hcid, m.selection ) of
        ( Settings, _ ) ->
            settingsHovercardView vc m hc

        ( Annotation, Pathfinder.SelectedAddress id ) ->
            annotationHovercardView vc id (Annotations.getAnnotation id m.annotations) hc

        ( Annotation, Pathfinder.SelectedTx id ) ->
            annotationHovercardView vc id (Annotations.getAnnotation id m.annotations) hc

        _ ->
            none


annotationHovercardView : View.Config -> Id -> Maybe Annotations.AnnotationItem -> Hovercard.Model -> Html Msg
annotationHovercardView vc id annotation hc =
    let
        labelValue =
            annotation |> Maybe.map .label |> Maybe.withDefault ""

        selectedColor =
            annotation |> Maybe.andThen .color

        inputField =
            input
                [ Sc.labelFieldStateActive_details.styles
                    ++ Sc.labelFieldStateActivePlaceholderText_details.styles
                    |> css
                , onInput (UserInputsAnnotation id)
                , HA.value labelValue
                , HA.placeholder (Locale.string vc.locale "Optional")
                , HA.autofocus True
                , HA.id "annotation-label-textbox"
                ]
                []

        colorBtn sColor color =
            let
                isSelected =
                    sColor == color
            in
            case color of
                Just c ->
                    Sc.colorSquareStyleColorFillWithAttributes
                        (Sc.colorSquareStyleColorFillAttributes
                            |> Rs.s_root [ css [ Css.cursor Css.pointer ], onClick (UserSelectsAnnotationColor id color) ]
                            |> Rs.s_vectorShape [ css [ Css.important (Css.fill (c |> Util.View.toCssColor)) ] ]
                        )
                        { root = { selectionVisible = isSelected } }

                Nothing ->
                    Sc.colorSquareStyleNoColorWithAttributes
                        (Sc.colorSquareStyleNoColorAttributes
                            |> Rs.s_root [ css [ Css.cursor Css.pointer ], onClick (UserSelectsAnnotationColor id Nothing) ]
                        )
                        { root = { selectionVisible = isSelected } }
    in
    Sc.annotationWithAttributes
        Sc.annotationAttributes
        { root = { colorText = Locale.string vc.locale "Color", labelText = Locale.string vc.locale "Label" }
        , labelField = { variant = inputField }
        , noColor = { variant = colorBtn selectedColor Nothing }
        , color1 = { variant = colorBtn selectedColor (Just Colors.annotation1_color) }
        , color2 = { variant = colorBtn selectedColor (Just Colors.annotation2_color) }
        , color3 = { variant = colorBtn selectedColor (Just Colors.annotation3_color) }
        , color4 = { variant = colorBtn selectedColor (Just Colors.annotation4_color) }
        , color5 = { variant = colorBtn selectedColor (Just Colors.annotation5_color) }
        , color6 = { variant = colorBtn selectedColor (Just Colors.annotation6_color) }
        , color7 = { variant = colorBtn selectedColor (Just Colors.annotation7_color) }
        , color8 = { variant = colorBtn selectedColor (Just Colors.annotation8_color) }
        }
        |> Html.toUnstyled
        |> List.singleton
        |> hovercard vc hc (Css.zIndexMainValue + 1)


settingsHovercardView : View.Config -> Pathfinder.Model -> Hovercard.Model -> Html Msg
settingsHovercardView vc pm hc =
    let
        switchWithText primary text enabled msg =
            let
                toggle =
                    Vc.toggle
                        { size = SwitchSizeBig
                        , selected = enabled
                        , disabled = False
                        , msg = msg
                        }
            in
            if primary then
                Sc.textSwitchStylePrimary
                    { root = { label = Locale.string vc.locale text }
                    , switch = { variant = toggle }
                    }

            else
                Sc.textSwitchStyleSecondary
                    { root = { label = Locale.string vc.locale text }
                    , switch = { variant = toggle }
                    }
    in
    Sc.displayProperties
        { exactValueSwitch = { variant = switchWithText True "Show exact values" (vc.locale.valueDetail == Locale.Exact) (UserClickedToggleValueDetail |> ChangedDisplaySettingsMsg) }
        , amountInFiatSwitch = { variant = switchWithText True "Amount in Fiat" vc.showValuesInFiat (UserClickedToggleValueDisplay |> ChangedDisplaySettingsMsg) }
        , gridSwitch = { variant = switchWithText True "Snap to Grid" pm.config.snapToGrid (UserClickedToggleSnapToGrid |> ChangedDisplaySettingsMsg) }
        , highlightSwitch = { variant = switchWithText True "Highlight on graph" pm.config.highlightClusterFriends (UserClickedToggleHighlightClusterFriends |> ChangedDisplaySettingsMsg) }
        , settingsLabelOfClustersSettings = { settingsLabel = Locale.string vc.locale "Clusters" }
        , settingsLabelOfGeneralSettings = { settingsLabel = Locale.string vc.locale "Graph" }
        , settingsLabelOfTransactionsSettings = { settingsLabel = Locale.string vc.locale "Asset flows" }
        , timestampSwitch = { variant = switchWithText True "Show timestamp" vc.showTimestampOnTxEdge (UserClickedToggleShowTxTimestamp |> ChangedDisplaySettingsMsg) }
        , timezoneSwitch = { variant = switchWithText False "with zone code" vc.showTimeZoneOffset (UserClickedToggleShowTimeZoneOffset |> ChangedDisplaySettingsMsg) }
        , utcSwitch = { variant = switchWithText False "in UTC" (not vc.showDatesInUserLocale) (UserClickedToggleDatesInUserLocale |> ChangedDisplaySettingsMsg) }
        }
        |> Html.toUnstyled
        |> List.singleton
        |> hovercard vc hc (Css.zIndexMainValue + 1)


topRightPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Model -> Html Msg
topRightPanel plugins pluginStates vc model =
    div [ Css.topRightPanelStyle vc |> css ]
        [ detailsView plugins pluginStates vc model
        ]


graphActionsView : View.Config -> Pathfinder.Config -> Pathfinder.Model -> Html Msg
graphActionsView vc _ model =
    let
        dropdown =
            if model.helpDropdownOpen then
                [ div
                    [ [ Css.position Css.fixed
                      , Css.top (Css.px 42)
                      , Css.zIndex (Css.int 100)
                      ]
                        |> css
                    , onClick UserClosesContextMenu
                    , stopPropagationOn "click" (Json.Decode.succeed ( UserClosesContextMenu, True ))
                    ]
                    [ HGraphComponents.rightClickMenuWithAttributes
                        (HGraphComponents.rightClickMenuAttributes
                            |> Rs.s_pluginsList [ [ Css.width (Css.pct 100) ] |> css ]
                            |> Rs.s_shortcutList [ [ Css.width (Css.pct 100) ] |> css ]
                            |> Rs.s_dividerLine [ [ Css.display Css.none ] |> css ]
                        )
                        { shortcutList =
                            [ { icon = HIcons.iconsInfoS {}
                              , text1 = "Legend"
                              , text2 = Nothing
                              , msg = UserClickedShowLegend
                              }
                                |> ContextMenuItem.init2
                                |> ContextMenuItem.view vc
                            , { link = "https://www.iknaio.com/learning#pathfinder20"
                              , icon = HIcons.iconsVideoS {}
                              , text1 = "Watch tutorials"
                              , text2 = Nothing
                              , blank = True
                              }
                                |> ContextMenuItem.initLink2
                                |> ContextMenuItem.view vc
                            ]
                        , pluginsList = []
                        }
                        {}
                    ]
                ]

            else
                []
    in
    div [ Css.graphActionsViewStyle vc |> css, stopPropagationOn "click" (Json.Decode.succeed ( NoOp, True )) ]
        (div [ Util.View.pointer, onClick UserClickedToggleHelpDropdown ]
            [ HIcons.framedIcon { root = { iconInstance = HIcons.iconsHelpOutlined {} } }
            ]
            :: dropdown
        )


searchBoxView : Plugins -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> Html Msg
searchBoxView plugins vc _ model =
    Sc.searchBarFieldStateTypingWithInstances
        Sc.searchBarFieldStateTypingAttributes
        (Sc.searchBarFieldStateTypingInstances
            |> Rs.s_searchInputField
                (View.Search.searchWithMoreCss plugins
                    vc
                    (View.Search.default
                        |> Rs.s_css
                            (\_ ->
                                Css.outline Css.none
                                    :: Css.pseudoClass "placeholder" Sc.searchBarFieldStatePlaceholderSearchInputField_details.styles
                                    :: (Css.width <| Css.pct 100)
                                    :: Sc.searchBarFieldStateTypingSearchInputField_details.styles
                                    ++ Sc.searchBarFieldStateTypingSearchText_details.styles
                            )
                        |> Rs.s_formCss
                            [ Css.flexGrow <| Css.num 1
                            , Css.height Css.auto |> Css.important
                            ]
                        |> Rs.s_frameCss
                            [ Css.height <| Css.pct 100
                            , Css.marginRight Css.zero |> Css.important
                            ]
                        |> Rs.s_resultLine
                            [ Css.property "background-color" Colors.white
                            , Css.hover
                                [ Css.property "background-color" Colors.greyBlue50
                                    |> Css.important
                                ]
                            ]
                        |> Rs.s_resultLineHighlighted
                            [ Css.property "background-color" Colors.greyBlue50
                            ]
                        |> Rs.s_resultsAsLink True
                        |> Rs.s_dropdownResult
                            [ Css.property "background-color" Colors.white
                            ]
                        |> Rs.s_dropdownFrame
                            [ Css.property "background-color" Colors.white
                            ]
                    )
                    model.search
                    |> Html.map SearchMsg
                    |> Just
                )
        )
        {}


detailsView : Plugins -> ModelState -> View.Config -> Pathfinder.Model -> Html Msg
detailsView plugin pluginStates vc model =
    case model.details of
        Just details ->
            case details of
                Pathfinder.AddressDetails id state ->
                    RemoteData.unwrap
                        (Util.View.loadingSpinner vc Css.View.loadingSpinner)
                        (AddressDetails.view plugin pluginStates vc model id)
                        state

                Pathfinder.TxDetails id state ->
                    TxDetails.view vc model id state

        Nothing ->
            none


graphSvg : Plugins -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> BBox -> Svg Msg
graphSvg plugins vc gc model bbox =
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

        gradient prefix { outgoing, reverse } =
            let
                ( from, to ) =
                    case ( outgoing, reverse ) of
                        ( True, False ) ->
                            ( Colors.pathMiddle, Colors.pathOut )

                        ( False, False ) ->
                            ( Colors.pathIn, Colors.pathMiddle )

                        ( True, True ) ->
                            ( Colors.pathOut, Colors.pathMiddle )

                        ( False, True ) ->
                            ( Colors.pathMiddle, Colors.pathIn )

                ( fromOffset, toOffset ) =
                    if outgoing then
                        ( "10%", "50%" )

                    else
                        ( "50%", "80%" )
            in
            linearGradient
                [ "{{ prefix }}{{ direction }}Edge{{ reverse }}"
                    |> String.Format.namedValue "prefix" prefix
                    |> String.Format.namedValue "direction"
                        (if outgoing then
                            "Out"

                         else
                            "In"
                        )
                    |> String.Format.namedValue "reverse"
                        (if reverse then
                            "Back"

                         else
                            "Forth"
                        )
                    |> id
                ]
                [ stop
                    [ from
                        |> stopColor
                    , offset fromOffset
                    ]
                    []
                , stop
                    [ offset toOffset
                    , to
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
         , Util.Graph.decodeCoords Coords.Coords
            |> Json.Decode.map (\_ -> ( NoOp, True ))
            |> preventDefaultOn "contextmenu"
         , Util.View.noTextSelection
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
            [ gradient "utxo" { outgoing = True, reverse = False }
            , gradient "utxo" { outgoing = False, reverse = False }
            , gradient "utxo" { outgoing = True, reverse = True }
            , gradient "utxo" { outgoing = False, reverse = True }
            , gradient "account" { outgoing = True, reverse = False }
            , gradient "account" { outgoing = False, reverse = False }
            , gradient "account" { outgoing = True, reverse = True }
            , gradient "account" { outgoing = False, reverse = True }
            ]
        , Svg.lazy7 Network.addresses plugins vc gc model.colors model.clusters model.annotations model.network.addresses
        , Svg.lazy5 Network.txs plugins vc gc model.annotations model.network.txs
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
                    |> Rs.s_root
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
