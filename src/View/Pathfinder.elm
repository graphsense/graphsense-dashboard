module View.Pathfinder exposing (view)

import Basics.Extra exposing (flip)
import Components.ExportCSV as ExportCSV
import Config.Pathfinder as Pathfinder exposing (TracingMode(..))
import Config.View as View
import Css
import Css.Graph
import Css.Pathfinder as Css
import Dict
import Hovercard
import Html.Styled as Html exposing (Html, div, input)
import Html.Styled.Attributes as HA
import Html.Styled.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave, preventDefaultOn, stopPropagationOn)
import Json.Decode
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords as Coords exposing (BBox, Coords)
import Model.Graph.Transform exposing (Transition(..))
import Model.Locale as Locale
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.ContextMenu as ContextMenu exposing (ContextMenu)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tools exposing (PointerTool(..), ToolbarHovercardModel, ToolbarHovercardType(..))
import Msg.Pathfinder exposing (DisplaySettingsMsg(..), Msg(..), OverlayWindows(..))
import Number.Bounded exposing (value)
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import Sha256
import String.Format
import Svg.Styled exposing (Svg, defs, feComposite, feFlood, feGaussianBlur, feMerge, feMergeNode, feOffset, filter, linearGradient, stop, svg)
import Svg.Styled.Attributes exposing (css, dx, dy, floodColor, height, id, in2, in_, offset, operator, preserveAspectRatio, result, stdDeviation, stopColor, transform, viewBox, width, x, y)
import Svg.Styled.Events as Svg
import Svg.Styled.Lazy as Svg
import Theme.Colors as Colors
import Theme.Html.GraphComponents as HGraphComponents
import Theme.Html.GraphComponentsAggregatedTracing as GraphComponentsAggregatedTracing
import Theme.Html.Icons as HIcons
import Theme.Html.SelectionControls exposing (SwitchSize(..))
import Theme.Html.SettingsComponents as Sc
import Theme.Svg.GraphComponents as GraphComponents
import Update.Graph.Transform as Transform
import Util.Annotations as Annotations
import Util.Css as Css
import Util.ExternalLinks
import Util.Graph
import Util.View exposing (fixFillRule, hovercard, none)
import View.Controls as Controls
import View.Graph.Transform as Transform
import View.Locale as Locale
import View.Pathfinder.AddressDetails as AddressDetails
import View.Pathfinder.ContextMenuItem as ContextMenuItem
import View.Pathfinder.ConversionDetails as ConversionDetails
import View.Pathfinder.Network as Network
import View.Pathfinder.RelationDetails as RelationDetails
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
    , topCenterPanel plugins pluginStates vc gc model
    , topRightPanel plugins pluginStates vc model
    , bottomCenterPanel vc model
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
                            |> ContextMenuItem.setDisabled
                                (case model.selection of
                                    Pathfinder.SelectedAddress _ ->
                                        False

                                    _ ->
                                        True
                                )
                            |> ContextMenuItem.view vc

                        -- , { msg = UserClickedContextMenuAlignVertically
                        --   , icon = HIcons.iconsLine {}
                        --   , text = Locale.string vc.locale "align vertically"
                        --   }
                        --     |> ContextMenuItem.init
                        --     |> ContextMenuItem.setDisabled
                        --         (case model.selection of
                        --             Pathfinder.MultiSelect _ ->
                        --                 False
                        --             _ ->
                        --                 True
                        --         )
                        --     |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuAlignHorizontally
                          , icon = HIcons.iconsHorizontalAlign {}
                          , text = Locale.string vc.locale "align horizontally"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.setDisabled
                                (case model.selection of
                                    Pathfinder.MultiSelect _ ->
                                        False

                                    _ ->
                                        True
                                )
                            |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuIdToClipboard menu
                          , icon = HIcons.iconsCopyS {}
                          , text = "copy address ID"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.setDisabled
                                (case model.selection of
                                    Pathfinder.SelectedAddress _ ->
                                        False

                                    _ ->
                                        True
                                )
                            |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuDeleteIcon menu
                          , icon = HIcons.iconsDeleteS {}
                          , text = "remove from graph"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuOpenInNewTab menu
                          , icon = HIcons.iconsGoToS {}
                          , text = "Open in new tab"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.setDisabled
                                (case model.selection of
                                    Pathfinder.SelectedAddress _ ->
                                        False

                                    _ ->
                                        True
                                )
                            |> ContextMenuItem.view vc
                        , { msg = UserOpensDialogWindow (AddTags id)
                          , icon = HIcons.iconsAddTagOutlinedS {}
                          , text = "report a tag"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.setDisabled
                                (case model.selection of
                                    Pathfinder.SelectedAddress _ ->
                                        False

                                    _ ->
                                        True
                                )
                            |> ContextMenuItem.view vc
                        ]
                    , pluginsList = pluginsList
                    }
                    {}

            ContextMenu.TransactionContextMenu id ->
                let
                    pluginsList =
                        []
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
                        [ { msg = UserOpensTxAnnotationDialog id
                          , icon = HIcons.iconsAnnotateS {}
                          , text = "Annotate transaction"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.view vc

                        -- , { msg = UserClickedContextMenuAlignVertically
                        --   , icon = HIcons.iconsLine {}
                        --   , text = Locale.string vc.locale "align vertically"
                        --   }
                        --     |> ContextMenuItem.init
                        --     |> ContextMenuItem.setDisabled
                        --         (case model.selection of
                        --             Pathfinder.MultiSelect _ ->
                        --                 False
                        --             _ ->
                        --                 True
                        --         )
                        --     |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuAlignHorizontally
                          , icon = HIcons.iconsHorizontalAlign {}
                          , text = Locale.string vc.locale "align horizontally"
                          }
                            |> ContextMenuItem.init
                            |> ContextMenuItem.setDisabled
                                (case model.selection of
                                    Pathfinder.MultiSelect _ ->
                                        False

                                    _ ->
                                        True
                                )
                            |> ContextMenuItem.view vc
                        , { msg = UserClickedContextMenuIdToClipboard menu
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
                            |> ContextMenuItem.setDisabled
                                (case model.selection of
                                    Pathfinder.SelectedTx _ ->
                                        False

                                    _ ->
                                        True
                                )
                            |> ContextMenuItem.view vc
                        ]
                    , pluginsList = pluginsList
                    }
                    {}
        ]


bottomCenterPanel : View.Config -> Pathfinder.Model -> Html Msg
bottomCenterPanel vc model =
    let
        text =
            case model.config.tracingMode of
                TransactionTracingMode ->
                    Locale.string vc.locale "tx-tracing-mode-help-text"

                AggregateTracingMode ->
                    Locale.string vc.locale "tx-relationship-mode-help-text"

        ctx =
            { text = text, domId = Sha256.sha256 text }
    in
    div
        [ css Css.bottomCenterPanelStyle
        , [ Css.property "gap" "10px" ] |> css
        ]
        [ GraphComponentsAggregatedTracing.traceModeToggleWithInstances
            (GraphComponentsAggregatedTracing.traceModeToggleAttributes
                |> Rs.s_root [ css [ Css.pointerEvents Css.visible ] ]
            )
            (GraphComponentsAggregatedTracing.traceModeToggleInstances
                |> Rs.s_toggleSwitchText
                    (Controls.toggleWithText
                        { selectedA = model.config.tracingMode == TransactionTracingMode
                        , titleA = Locale.string vc.locale "transaction-based" --"Track funds"
                        , titleB = Locale.string vc.locale "relationship-based" -- "View network"
                        , msg = UserClickedToggleTracingMode
                        }
                        |> Just
                    )
            )
            { leftCell = { variant = none }
            , rightCell = { variant = none }
            , root = { toggleLabel = "" }
            }
        , HIcons.framedIconCircleWithAttributes
            (HIcons.framedIconCircleAttributes
                |> Rs.s_root
                    [ onMouseEnter (ShowTextTooltip ctx)
                    , onMouseLeave (CloseTextTooltip ctx)
                    , HA.id ctx.domId
                    , css [ Css.pointerEventsAll ]
                    ]
            )
            { root =
                { iconInstance =
                    HIcons.iconsInfoLWithAttributes
                        (HIcons.iconsInfoLAttributes
                            |> Rs.s_root [ fixFillRule ]
                        )
                        {}
                }
            }
        ]


topCenterPanel : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> Html Msg
topCenterPanel plugins pluginStates vc gc model =
    div
        [ css Css.topPanelStyle
        ]
        [ div [ css [ Css.property "pointer-events" "all", Css.position Css.relative, Css.overflow Css.visible ] ]
            (Plugin.pathfinderUpperLeftPanel plugins pluginStates vc)
        , div
            [ css
                [ Css.displayFlex
                , Css.property "gap" "10px"
                , Css.property "pointer-events" "all"
                , Css.flexWrap Css.wrap
                ]
            ]
            [ searchBoxView plugins vc gc model
            , Toolbar.view vc
                { undoDisabled = List.isEmpty model.history.past
                , redoDisabled = List.isEmpty model.history.future
                , deleteDisabled =
                    case model.selection of
                        Pathfinder.NoSelection ->
                            True

                        Pathfinder.SelectedConversionEdge _ ->
                            True

                        _ ->
                            False
                , newDisabled = not model.isDirty
                , alignHorizontalDisabled =
                    case model.selection of
                        Pathfinder.MultiSelect _ ->
                            False

                        _ ->
                            True
                , annotateDisabled =
                    case model.selection of
                        Pathfinder.SelectedAddress _ ->
                            False

                        Pathfinder.SelectedTx _ ->
                            False

                        Pathfinder.MultiSelect _ ->
                            False

                        _ ->
                            True
                , pointerTool = model.pointerTool
                , exportName = model.name
                , exportCSV = ExportCSV.isDownloading model.exportCSVGraph
                , exportPNG = model.exportPNG
                , exportPDF = model.exportPDF
                }
            ]
        , div
            [ css [ Css.property "pointer-events" "all", Css.position Css.relative, Css.overflow Css.visible ] ]
            [ graphActionsView vc gc model
            ]
        ]


toolbarHovercardView : View.Config -> Pathfinder.Model -> ToolbarHovercardModel -> Html Msg
toolbarHovercardView vc m ( hcid, hc ) =
    case ( hcid, m.selection ) of
        ( Settings, _ ) ->
            settingsHovercardView vc m hc

        ( Annotation, Pathfinder.SelectedAddress id ) ->
            annotationHovercardView vc [ id ] (Annotations.getAnnotation id m.annotations) hc

        ( Annotation, Pathfinder.SelectedTx id ) ->
            annotationHovercardView vc [ id ] (Annotations.getAnnotation id m.annotations) hc

        ( Annotation, Pathfinder.MultiSelect selections ) ->
            let
                ids =
                    List.map
                        (\sel ->
                            case sel of
                                Pathfinder.MSelectedAddress id ->
                                    id

                                Pathfinder.MSelectedTx id ->
                                    id
                        )
                        selections

                -- Use first ID's annotation to track the current input value
                firstAnnotation =
                    List.head ids
                        |> Maybe.andThen (\id -> Annotations.getAnnotation id m.annotations)
            in
            annotationHovercardView vc ids firstAnnotation hc

        _ ->
            none


annotationHovercardView : View.Config -> List Id -> Maybe Annotations.AnnotationItem -> Hovercard.Model -> Html Msg
annotationHovercardView vc ids annotation hc =
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
                , onInput (UserInputsAnnotation ids)
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
                            |> Rs.s_root [ css [ Css.cursor Css.pointer ], onClick (UserSelectsAnnotationColor ids color) ]
                            |> Rs.s_vectorShape [ css [ Css.important (Css.fill (c |> Util.View.toCssColor)) ] ]
                        )
                        { root = { selectionVisible = isSelected } }

                Nothing ->
                    Sc.colorSquareStyleNoColorWithAttributes
                        (Sc.colorSquareStyleNoColorAttributes
                            |> Rs.s_root [ css [ Css.cursor Css.pointer ], onClick (UserSelectsAnnotationColor ids Nothing) ]
                        )
                        { root = { selectionVisible = isSelected } }
    in
    Sc.annotationWithAttributes
        Sc.annotationAttributes
        { root = { colorText = Locale.string vc.locale "color", labelText = Locale.string vc.locale "Label" }
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
        , color9 = { variant = colorBtn selectedColor (Just Colors.annotation9_color) }
        , color10 = { variant = colorBtn selectedColor (Just Colors.annotation10_color) }
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
                    Controls.toggle
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
        , showBothValuesSwitch = { variant = switchWithText True "Show both on tx" vc.showBothValues (UserClickedToggleBothValueDisplay |> ChangedDisplaySettingsMsg) }
        , gridSwitch = { variant = switchWithText True "snap to grid" pm.config.snapToGrid (UserClickedToggleSnapToGrid |> ChangedDisplaySettingsMsg) }
        , highlightSwitch = { variant = switchWithText True "highlight on graph" pm.config.highlightClusterFriends (UserClickedToggleHighlightClusterFriends |> ChangedDisplaySettingsMsg) }
        , avoidOverlapingNodes = { variant = switchWithText True "avoid overlapping nodes" pm.config.avoidOverlapingNodes (UserClickedToggleAvoidOverlapingNodes |> ChangedDisplaySettingsMsg) }
        , settingsLabelOfClustersSettings = { settingsLabel = Locale.string vc.locale "Clusters" }
        , settingsLabelOfGraphSettings = { settingsLabel = Locale.string vc.locale "Graph" }
        , settingsLabelOfValueSettings = { settingsLabel = Locale.string vc.locale "Assets" }
        , settingsLabelOfTimeSettings = { settingsLabel = Locale.string vc.locale "Time" }
        , timestampSwitch = { variant = switchWithText True "Show timestamp" vc.showTimestampOnTxEdge (UserClickedToggleShowTxTimestamp |> ChangedDisplaySettingsMsg) }
        , timezoneSwitch = { variant = switchWithText True "With zone code" vc.showTimeZoneOffset (UserClickedToggleShowTimeZoneOffset |> ChangedDisplaySettingsMsg) }
        , utcSwitch = { variant = switchWithText True "In UTC" (not vc.showDatesInUserLocale) (UserClickedToggleDatesInUserLocale |> ChangedDisplaySettingsMsg) }
        , showHash = { variant = switchWithText True "Show transaction hash" vc.showHash (UserClickedToggleShowHash |> ChangedDisplaySettingsMsg) }
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
                    [ [ Css.position Css.absolute
                      , Css.top (Css.px HIcons.framedIcon_details.height)
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
                            [ { icon = HIcons.iconsInfoSWithAttributes (HIcons.iconsInfoSAttributes |> Rs.s_root [ fixFillRule ]) {}
                              , text1 = "Legend"
                              , text2 = Nothing
                              , msg = UserClickedShowLegend
                              }
                                |> ContextMenuItem.init2
                                |> ContextMenuItem.view vc
                            , { link = "https://www.iknaio.com/learning#pathfinder20"
                              , icon = HIcons.iconsVideoSWithAttributes (HIcons.iconsVideoSAttributes |> Rs.s_root [ fixFillRule ]) {}
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
                    AddressDetails.view plugin pluginStates vc model id state

                Pathfinder.TxDetails id state ->
                    TxDetails.view vc model id state

                Pathfinder.RelationDetails id state ->
                    RelationDetails.view vc model id state

                Pathfinder.ConversionDetails id state ->
                    ConversionDetails.view vc id (flip Dict.member model.network.txs) state

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
         , id Pathfinder.graphId
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
            , dropShadowEdgeHighlight
            ]
        , Svg.lazy7 Network.relations plugins vc gc model.annotations model.network.txs model.network.aggEdges model.network.conversions
        , Svg.lazy7 Network.addresses plugins vc gc model.colors model.clusters model.annotations model.network.addresses
        , drawDragSelector vc model

        -- , rect [ fill "red", width "3", height "3", x "0", y "0" ] [] -- Mark zero point in coordinate system
        -- , showBoundingBox model
        ]


dropShadowEdgeHighlight : Svg Msg
dropShadowEdgeHighlight =
    filter
        [ id "dropShadowEdgeHighlight"
        , x "-100%"
        , y "-200%"
        , width "300%"
        , height "500%"
        ]
        [ feGaussianBlur
            [ in_ "SourceAlpha"
            , stdDeviation "10"
            ]
            []
        , feOffset
            [ dx "0"
            , dy "2"
            , result "offsetblur"
            ]
            []
        , feFlood
            [ floodColor "rgba(0, 0, 0, 0.25)"
            ]
            []
        , feComposite
            [ in2 "offsetblur"
            , operator "in"
            ]
            []
        , feMerge
            []
            [ feMergeNode
                []
                []
            , feMergeNode
                [ in_ "SourceGraphic"
                ]
                []
            ]
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
