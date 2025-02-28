module Util.Tag exposing (Msg(..), TooltipContext, conceptItem)

import Config.View as View
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Events exposing (onMouseEnter, onMouseLeave)
import Json.Encode
import Model.Pathfinder.Id as Id exposing (Id)
import Theme.Html.TagsComponents as TagComponents


type alias TooltipContext =
    { context : String, domId : String }


type Msg
    = UserMovesMouseOverTagConcept TooltipContext
    | UserMovesMouseOutTagConcept TooltipContext


conceptItem : View.Config -> Id -> String -> Html Msg
conceptItem vc id k =
    let
        ctx =
            { context =
                [ k, Id.network id, Id.id id ]
                    |> Json.Encode.list Json.Encode.string
                    |> Json.Encode.encode 0
            , domId = k ++ Id.id id ++ "_tags_concept_tag"
            }
    in
    Html.div
        [ onMouseEnter (UserMovesMouseOverTagConcept ctx)
        , onMouseLeave (UserMovesMouseOutTagConcept ctx)
        , HA.id ctx.domId
        , css [ Css.cursor Css.pointer ]
        ]
        [ TagComponents.categoryTags
            { categoryTags =
                { tagLabel =
                    View.getConceptName vc k |> Maybe.withDefault k
                }
            }
        ]
