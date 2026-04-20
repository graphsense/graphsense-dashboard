module View.Pathfinder.TagDetailsList exposing (view)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Tooltip as Tooltip
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth)
import Css.View
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Model exposing (Msg(..))
import Model.Dialog as Dialog
import Model.Pathfinder.Id as Id
import Msg.Pathfinder
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
import Theme.Html.TagsComponents as TagsComponents
import Util.Tooltip as Util
import Util.TooltipType
import Util.View exposing (copyIconPathfinder, none, onClickWithStop)
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
                    |> Rs.s_tagIcon
                        [ css
                            (if isClusterTab then
                                [ Css.property "fill" Colors.greyBlue500_string |> Css.important ]

                             else
                                []
                            )
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

        disabledTabStyle =
            [ Css.cursor Css.default
            , Css.padding2 (Css.px 4) (Css.px 12)
            , Css.fontSize (Css.px 14)
            , Css.property "border-bottom" "none"
            , Css.property "color" Colors.greyBlue400
            , Css.fontWeight (Css.int 400)
            , Css.opacity (Css.num 0.5)
            ]

        addressTab =
            if conf.hasAddressTags then
                div
                    [ css (tabStyle (conf.activeTab == Dialog.AddressTagsTab))
                    , onClickWithStop (UserClickedTagsDialogTab Dialog.AddressTagsTab)
                    ]
                    [ Html.Styled.text (Locale.string vc.locale "address tags") ]

            else
                div
                    [ css disabledTabStyle ]
                    [ Html.Styled.text (Locale.string vc.locale "address tags") ]

        clusterTabTooltipConfig =
            Util.tooltipConfig vc (\tipMsg -> PathfinderMsg (Msg.Pathfinder.TooltipMsg tipMsg))
                |> Tooltip.withFixed

        clusterTab =
            div
                [ css (tabStyle (conf.activeTab == Dialog.ClusterTagsTab) ++ [ Css.displayFlex, Css.alignItems Css.center, Css.property "gap" "4px" ])
                , onClickWithStop (UserClickedTagsDialogTab Dialog.ClusterTagsTab)
                ]
                [ Html.Styled.text (Locale.string vc.locale "cluster tags")
                , Icons.iconsInfoSnoPaddingDevWithAttributes
                    (Icons.iconsInfoSnoPaddingDevAttributes
                        |> Rs.s_root
                            (css [ Css.width (Css.px 16), Css.height (Css.px 16) ]
                                :: (Util.TooltipType.Text "cluster tags disclaimer"
                                        |> Tooltip.attributes "cluster-tab-tooltip" clusterTabTooltipConfig
                                   )
                            )
                    )
                    {}
                ]

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
                        (TagsComponents.dialogTagsListComponentTabs_details.styles
                            ++ [ Css.width (Css.pct 100) ]
                        )
                    ]
                    tabItems

            else
                Html.Styled.text ""

        tableContent =
            case conf.activeTab of
                Dialog.AddressTagsTab ->
                    tagsInfiniteTable vc TagsListDialogAddressTableMsg conf.addressTagsTable

                Dialog.ClusterTagsTab ->
                    case conf.clusterTagsState of
                        Dialog.ClusterTagsLoaded table ->
                            tagsInfiniteTable vc TagsListDialogClusterTableMsg table

                        Dialog.ClusterTagsLoading ->
                            Util.View.loadingSpinner vc Css.View.loadingSpinner

                        Dialog.ClusterTagsNotLoaded ->
                            Html.Styled.text ""
    in
    div
        [ css
            [ Css.minWidth (Css.px 1000)
            , Css.minHeight (Css.px 600)
            ]
        ]
        [ div
            [ css
                (TagsComponents.dialogTagsListComponent_details.styles ++ [ Css.width (Css.pct 100) ])
            ]
            [ header
            , tabs
            , tableContent
            ]
        ]


tagsInfiniteTable : View.Config -> (InfiniteTable.Msg -> Msg) -> InfiniteTable.Model Api.Data.AddressTag -> Html Msg
tagsInfiniteTable vc tag tbl =
    if InfiniteTable.isEmpty tbl then
        if InfiniteTable.isLoading tbl then
            Util.View.loadingSpinner vc Css.View.loadingSpinner

        else
            Html.Styled.text ""

    else
        InfiniteTable.view (TagsTable.config vc tag)
            [ css (Css.maxHeight (Css.px 500) :: fullWidth) ]
            tbl
