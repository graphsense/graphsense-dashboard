module View.Pathfinder.TagDetailsList exposing (view)

import Api.Data
import Components.Table exposing (Table)
import Config.View as View
import Css
import Css.View
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Model exposing (Msg(..))
import Model.Dialog as Dialog
import Model.Pathfinder.Id as Id
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
import Theme.Html.TagsComponents as TagsComponents
import Util.View exposing (copyIconPathfinder, none, onClickWithStop)
import View.Graph.Table
import View.Locale as Locale
import View.Pathfinder.Table.TagsTable as TagsTable


view : View.Config -> Dialog.TagListConfig Msg -> Html Msg
view vc conf =
    let
        fullWidthAttr =
            Css.pct 100 |> Css.width |> List.singleton |> css |> List.singleton

        isClusterTab =
            conf.activeTab == Dialog.ClusterTagsTab

        headerInstances =
            if isClusterTab then
                TagsComponents.dialogTagHeaderInstances
                    |> Rs.s_icon
                        (Just
                            (Icons.iconsTagLTypeIndirectWithAttributes
                                (Icons.iconsTagLTypeIndirectAttributes
                                    |> Rs.s_root
                                        [ css
                                            [ Css.width (Css.px 40)
                                            , Css.height (Css.px 40)
                                            ]
                                        ]
                                )
                                {}
                            )
                        )

            else
                TagsComponents.dialogTagHeaderInstances

        header =
            TagsComponents.dialogTagHeaderWithInstances
                (TagsComponents.dialogTagHeaderAttributes
                    |> Rs.s_root fullWidthAttr
                    |> Rs.s_header fullWidthAttr
                    |> Rs.s_icon
                        [ css
                            [ Css.displayFlex
                            , Css.alignItems Css.center
                            , Css.justifyContent Css.center
                            ]
                        ]
                    |> Rs.s_closeIcon [ [ Css.cursor Css.pointer ] |> css, onClickWithStop conf.closeMsg ]
                )
                headerInstances
                { root =
                    { headerTitle = Locale.string vc.locale "tags list"
                    }
                , identifierWithCopyIcon =
                    { chevronInstance = none
                    , copyIconInstance = Id.id conf.id |> copyIconPathfinder vc
                    , identifier = Id.id conf.id
                    , addTagIconInstance = none
                    }
                }

        tColor isActive =
            if isActive then
                Colors.blue400

            else
                Colors.greyBlue400

        tabStyle isActive =
            [ Css.cursor Css.pointer
            , Css.padding2 (Css.px 4) (Css.px 12)
            , Css.fontSize (Css.px 14)
            , Css.property "border-bottom"
                (if isActive then
                    "2px solid " ++ tColor isActive

                 else
                    "none"
                )
            , Css.property "color"
                (tColor isActive)
            , Css.fontWeight
                (Css.int
                    (if isActive then
                        600

                     else
                        400
                    )
                )
            ]

        addressTab =
            div
                [ css (tabStyle (conf.activeTab == Dialog.AddressTagsTab))
                , onClickWithStop (UserClickedTagsDialogTab Dialog.AddressTagsTab)
                ]
                [ Html.Styled.text (Locale.string vc.locale "address tags") ]

        clusterTab =
            div
                [ css (tabStyle (conf.activeTab == Dialog.ClusterTagsTab))
                , onClickWithStop (UserClickedTagsDialogTab Dialog.ClusterTagsTab)
                ]
                [ Html.Styled.text (Locale.string vc.locale "cluster tags") ]

        tabItems =
            case ( conf.showAddressTab, conf.showClusterTab ) of
                ( True, True ) ->
                    [ addressTab, clusterTab ]

                ( True, False ) ->
                    [ addressTab ]

                ( False, True ) ->
                    [ clusterTab ]

                ( False, False ) ->
                    []

        tabs =
            if List.length tabItems > 1 then
                div
                    [ css
                        (TagsComponents.dialogTagsListComponentDevTabs_details.styles
                            ++ [ Css.width (Css.pct 100) ]
                        )
                    ]
                    tabItems

            else
                Html.Styled.text ""

        disclaimer =
            div
                [ css
                    [ Css.width (Css.pct 100)
                    , Css.margin2 (Css.px 16) (Css.px 0)
                    , Css.property "color" Colors.red500
                    ]
                ]
                [ Html.Styled.text (Locale.string vc.locale "cluster tags disclaimer") ]

        tableContent =
            case conf.activeTab of
                Dialog.AddressTagsTab ->
                    tagsTable vc conf.addressTagsTable

                Dialog.ClusterTagsTab ->
                    case conf.clusterTagsState of
                        Dialog.ClusterTagsLoaded table ->
                            tagsTable vc table

                        Dialog.ClusterTagsLoading ->
                            Util.View.loadingSpinner vc Css.View.loadingSpinner

                        Dialog.ClusterTagsNotLoaded ->
                            Html.Styled.text ""
    in
    div
        [ css
            (TagsComponents.dialogTagsListComponentDev_details.styles ++ [ Css.width (Css.pct 100) ])
        ]
        [ header
        , tabs
        , tableContent
        , if isClusterTab then
            disclaimer

          else
            Html.Styled.text ""
        ]


tagsTable : View.Config -> Table Api.Data.AddressTag -> Html Msg
tagsTable vc tags =
    View.Graph.Table.table
        TagsTable.styles
        vc
        [ css
            [ Css.verticalAlign Css.top
            , Css.overflowY Css.scroll
            , Css.overflowX Css.hidden
            , Css.minWidth (Css.px 500)
            , Css.minHeight (Css.px 300)
            ]
        ]
        View.Graph.Table.noTools
        (TagsTable.config vc)
        tags
