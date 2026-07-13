module Util.Tag exposing (actorItem, conceptItem)

import Components.Tooltip as Tooltip
import Config.View as View
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HA exposing (css)
import Model.Pathfinder.Id as Id exposing (Id)
import Theme.Html.Icons as HIcons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Theme.Html.TagsComponents as TagComponents
import Util.Tooltip
import Util.TooltipType exposing (TooltipType(..))
import Util.View


actorItem : View.Config -> Id -> (Tooltip.Msg TooltipType -> msg) -> String -> String -> Html msg
actorItem vc id tag actorId label =
    let
        domId =
            Id.id id ++ "_" ++ actorId ++ "_tags_actor"

        maxLblLength =
            15

        lbl_truncated =
            label |> Util.View.truncate maxLblLength

        tooltipAttributes =
            ActorDetails actorId
                |> Tooltip.attributes domId
                    (Util.Tooltip.tooltipConfig vc tag
                        |> Tooltip.withKeepOpenOnHover
                    )
    in
    Html.div
        (css
            (SidePanelComponents.sidePanelEthAddressActor_details.styles
                ++ [ Css.cursor Css.default
                   , Css.property "gap" "2px"
                   , Css.justifyContent Css.flexStart
                   ]
            )
            :: (if String.length label > maxLblLength then
                    [ HA.title label ]

                else
                    []
               )
            ++ tooltipAttributes
        )
        [ HIcons.iconsAssignWithAttributes
            { root =
                [ css
                    [ Css.transforms [ Css.scale 0.75 ]
                    , Css.marginLeft (Css.px -6)
                    , Css.marginRight (Css.px -6)
                    ]
                ]
            , user = []
            }
            {}
        , Html.div
            [ css
                (SidePanelComponents.sidePanelEthAddressLabelOfActor_details.styles
                    ++ [ Css.fontSize (Css.px 12) ]
                )
            ]
            [ Html.text lbl_truncated ]
        ]


conceptItem : View.Config -> Id -> (Tooltip.Msg TooltipType -> msg) -> String -> Html msg
conceptItem vc id tag k =
    let
        domId =
            k ++ Id.id id ++ "_tags_concept_tag"

        maxLblLength =
            15

        lbl =
            View.getConceptName vc k |> Maybe.withDefault k

        lbl_truncated =
            lbl |> Util.View.truncate maxLblLength

        tooltipAttributes =
            TagConcept id k
                |> Tooltip.attributes domId
                    (Util.Tooltip.tooltipConfig vc tag
                        |> Tooltip.withKeepOpenOnHover
                    )
    in
    Html.div
        (css [ Css.cursor Css.default ]
            :: (if String.length lbl > maxLblLength then
                    [ HA.title lbl ]

                else
                    []
               )
            ++ tooltipAttributes
        )
        [ TagComponents.categoryTag
            { root =
                { tagLabel = lbl_truncated
                , closeVisible = False
                }
            }
        ]
