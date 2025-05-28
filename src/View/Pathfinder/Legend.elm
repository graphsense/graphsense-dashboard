module View.Pathfinder.Legend exposing (ItemType, legendItem, legendView)

import Config.View as View
import Html.Styled exposing (Html)
import Html.Styled.Events exposing (onClick)
import Model exposing (Msg)
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import Theme.Html.Dialogs
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
    in
    case itemt of
        Node ->
            Theme.Html.Dialogs.legendItemNode data

        IconItem ->
            Theme.Html.Dialogs.legendItemIcon data


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
                { description = "Confirmed tag on this address."
                , icon = Icons.iconsTagL { root = { type_ = Icons.IconsTagLTypeDirect } }
                , label = "Direct Tag"
                }
            , legendItem vc
                IconItem
                { description = "Tag inferred via cluster association."
                , icon = Icons.iconsTagL { root = { type_ = Icons.IconsTagLTypeIndirect } }
                , label = "Indirect Tag"
                }
            ]
                ++ pluginLegendIconItems
                ++ [ legendItem vc
                        IconItem
                        { description = "The first address or transaction added to the graph."
                        , icon = Icons.iconsNodeMarker { root = { purpose = Icons.IconsNodeMarkerPurposeStartingPoint } }
                        , label = "Starting Point"
                        }
                   , legendItem vc
                        IconItem
                        { description = "The address or transaction currently selected."
                        , icon = Icons.iconsNodeMarker { root = { purpose = Icons.IconsNodeMarkerPurposeSelectedNode } }
                        , label = "You Are Here"
                        }
                   ]
        , graphNodesItemsList =
            [ legendItem vc
                Node
                { description = "No known identity."
                , icon = Icons.iconsUntaggesSnoPadding {}
                , label = "Unlabeled Address"
                }
            , legendItem vc
                Node
                { description = "Verified cryptoasset exchange."
                , icon = Icons.iconsExchangeSnoPadding {}
                , label = "Exchange"
                }
            , legendItem vc
                Node
                { description = "Likely a service (e.g. high activity, but not confirmed)"
                , icon = Icons.iconsUnknownServiceSnoPadding {}
                , label = "Possible Service"
                }
            , legendItem vc
                Node
                { description = "Known entity such as a business or institution."
                , icon = Icons.iconsInstitutionSnoPadding {}
                , label = "Institution"
                }
            , legendItem vc
                Node
                { description = "Programmed address for automated transactions."
                , icon = Icons.iconsSmartContractSnoPadding {}
                , label = "Smart Contract"
                }
            ]
        }
        { root =
            { header = Locale.string vc.locale "Visualization Legend"
            , header2 = Locale.string vc.locale "Graph Nodes"
            , header3 = Locale.string vc.locale "Additional Icons"
            }
        }
