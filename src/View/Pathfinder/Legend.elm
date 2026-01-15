module View.Pathfinder.Legend exposing (ItemType, legendItem, legendView)

import Config.View as View
import Css
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model exposing (Msg)
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import Theme.Html.Dialogs
import Theme.Html.GraphComponents as GraphComponents
import Theme.Html.Icons as Icons
import Util.View exposing (pointer)
import View.Locale as Locale


type ItemType
    = Node
    | IconItem


legendItem : View.Config -> ItemType -> { description : String, icon : Html msg, label : String } -> Html msg
legendItem vc itemt { description, icon, label } =
    let
        data =
            { root =
                { description = Locale.string vc.locale description
                , iconInstance = icon
                , label = Locale.string vc.locale label
                }
            }

        attrDesc =
            [ [ Css.whiteSpace Css.normal, Css.textAlign Css.left ] |> css ]
    in
    case itemt of
        Node ->
            Theme.Html.Dialogs.legendItemNodeWithAttributes
                (Theme.Html.Dialogs.legendItemNodeAttributes
                    |> Rs.s_description attrDesc
                )
                data

        IconItem ->
            Theme.Html.Dialogs.legendItemIconWithAttributes
                (Theme.Html.Dialogs.legendItemIconAttributes
                    |> Rs.s_description attrDesc
                )
                data


legendView : Plugins -> View.Config -> Msg -> Html Msg
legendView plugins vc closeMsg =
    let
        pluginLegendIconItems =
            Plugin.getLegendIconItems plugins vc |> List.map (legendItem vc IconItem)
    in
    Theme.Html.Dialogs.dialogLegendWithAttributes
        (Theme.Html.Dialogs.dialogLegendAttributes
            |> Rs.s_iconsCloseBlack [ pointer, onClick closeMsg ]
        )
        { additionalIconsItemsList =
            [ legendItem vc
                IconItem
                { description = "Attribution-tag-on-address"
                , icon = Icons.iconsTagL { root = { type_ = Icons.IconsTagLTypeDirect } }
                , label = "Direct tag"
                }
            , legendItem vc
                IconItem
                { description = "Attribution-tag-inferred"
                , icon = Icons.iconsTagL { root = { type_ = Icons.IconsTagLTypeIndirect } }
                , label = "Indirect tag"
                }
            ]
                ++ pluginLegendIconItems
                ++ [ legendItem vc
                        IconItem
                        { description = "Hint-first-thing-added"
                        , icon = Icons.iconsNodeMarker { root = { purpose = Icons.IconsNodeMarkerPurposeStartingPoint } }
                        , label = "Hint-starting-point"
                        }
                   , legendItem vc
                        IconItem
                        { description = "Hint-thing-selected"
                        , icon = Icons.iconsNodeMarker { root = { purpose = Icons.IconsNodeMarkerPurposeSelectedNode } }
                        , label = "You are here"
                        }
                   ]
        , graphNodesItemsList =
            [ legendItem vc
                Node
                { description = "No known identity"
                , icon = Icons.iconsUntaggesSnoPadding {}
                , label = "Unlabeled address"
                }
            , legendItem vc
                Node
                { description = "Known-exchange"
                , icon = Icons.iconsExchangeSnoPadding {}
                , label = "exchange"
                }
            , legendItem vc
                Node
                { description = "Hint-possible-service"
                , icon = Icons.iconsUnknownServiceSnoPadding {}
                , label = "Possible service"
                }
            , legendItem vc
                Node
                { description = "Hint-known-entity"
                , icon = Icons.iconsInstitutionSnoPadding {}
                , label = "institution"
                }
            , legendItem vc
                Node
                { description = "Hint-smart-contract"
                , icon = Icons.iconsSmartContractSnoPadding {}
                , label = "Smart contract"
                }
            , legendItem vc
                IconItem
                { description = "Hint-direct-transfer-of-asset"
                , icon = Icons.iconsUntagged {}
                , label = "Unlabeled transaction"
                }
            , legendItem vc
                IconItem
                { description = "Hint-simplified-address"
                , icon = GraphComponents.swapNode { root = { highlightInvisible = False } }
                , label = "Hint-swap-bridge"
                }
            ]
        }
        { root =
            { header = Locale.string vc.locale "Symbol guide"
            , header2 = Locale.string vc.locale "Graph nodes"
            , header3 = Locale.string vc.locale "Additional icons"
            }
        }
