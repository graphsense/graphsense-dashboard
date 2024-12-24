module View.Pathfinder.TagDetailsList exposing (view)

import Api.Data
import Config.View as View
import Css
import Css.Table
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Init.Pathfinder.Table.TagsTable as TagsTable
import Model exposing (Msg)
import Model.Pathfinder.Id as Id exposing (Id)
import RecordSetter as Rs
import Theme.Html.TagsComponents as TagsComponents
import Util.View exposing (copyIconPathfinder, none, onClickWithStop)
import View.Graph.Table
import View.Locale as Locale
import View.Pathfinder.Table.TagsTable as TagsTable


view : View.Config -> Id -> Maybe Api.Data.AddressTags -> Html Msg
view vc id tags =
    let
        header =
            TagsComponents.dialogTagHeader
                { dialogTagHeader = { headerTitle = Locale.string vc.locale "Tags list" }
                , identifierWithCopyIcon =
                    { chevronInstance = none
                    , copyIconInstance = Id.id id |> copyIconPathfinder vc
                    , identifier = Id.id id
                    }
                }

        styles =
            Css.Table.styles
    in
    div
        [ css
            (TagsComponents.dialogTagsListComponent_details.styles ++ [])
        ]
        [ header
        , View.Graph.Table.table
            styles
            vc
            [ css [ Css.overflowY Css.auto, Css.maxHeight (Css.px ((vc.size |> Maybe.map .height |> Maybe.withDefault 500) * 0.5)) ] ]
            View.Graph.Table.noTools
            (TagsTable.config vc)
            (TagsTable.init (tags |> Maybe.map .addressTags |> Maybe.withDefault []))
        ]



-- TagsComponents.dialogTagsListComponentWithInstances
--     TagsComponents.dialogTagsListComponentAttributes
--     (TagsComponents.dialogTagsListComponentInstances
--         |> Rs.s_dialogTagHeader (Just header)
--         |> Rs.s_dialogTagsListComponent (Just none)
--     )
--     { dialogTagHeader = { headerTitle = "dummy" }
--     , identifierWithCopyIcon =
--         { chevronInstance = none
--         , copyIconInstance = none
--         , identifier = "dummy"
--         }
--     , tagCellDate130 = cellDummy
--     , tagCellDate170 = cellDummy
--     , tagCellDate49 = cellDummy
--     , tagCellDate90 = cellDummy
--     , tagCellLabel100 = cellDummy
--     , tagCellLabel140 = cellDummy
--     , tagCellLabel19 = cellDummy
--     , tagCellLabel60 = cellDummy
--     , tagCellSource120 = cellDummy
--     , tagCellSource160 = cellDummy
--     , tagCellSource39 = cellDummy
--     , tagCellSource80 = cellDummy
--     , tagCellType110 = cellDummy
--     , tagCellType150 = cellDummy
--     , tagCellType29 = cellDummy
--     , tagCellType70 = cellDummy
--     , timeTitle = { checkboxVisible = False, sort = none }
--     }
--     |> List.singleton
--     |> div []
