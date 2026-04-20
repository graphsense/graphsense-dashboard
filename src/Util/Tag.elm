module Util.Tag exposing (conceptItem)

import Components.Tooltip as Tooltip
import Config.View as View
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HA exposing (css)
import Model.Pathfinder.Id as Id exposing (Id)
import Theme.Html.TagsComponents as TagComponents
import Util.Tooltip
import Util.TooltipType exposing (TooltipType(..))
import Util.View


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
                |> Tooltip.attributes domId (Util.Tooltip.tooltipConfig vc tag)
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
