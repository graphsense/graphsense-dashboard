module View.Graph.Tag exposing (..)

import Api.Data
import Config.View as View
import Css.Button
import Css.Graph as Css
import Css.View
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model.Graph.Tag exposing (..)
import Msg.Graph exposing (Msg(..))
import Plugin.View exposing (Plugins)
import Util.View exposing (none)
import View.Dialog as Dialog
import View.Locale as Locale
import View.Search as Search


type alias Config =
    { entityConcepts : List Api.Data.Concept
    , abuseConcepts : List Api.Data.Concept
    }


inputHovercard : Plugins -> View.Config -> Config -> Model -> Html Msg
inputHovercard plugins vc tc model =
    [ Just UserClicksCloseTagHovercard
        |> Dialog.headRow vc "Add tag"
    , Dialog.body vc
        { onSubmit = UserSubmitsTagInput }
        [ Dialog.part vc
            "Label"
            [ Search.search plugins
                vc
                { searchable = Search.SearchTagsOnly
                , css = \_ -> Css.searchTextarea vc
                , resultsAsLink = False
                , multiline = False
                , showIcon = False
                }
                model.input.label
                |> Html.map TagSearchMsg
            ]
        , Dialog.part vc
            "Source"
            [ input
                [ onInput UserInputsTagSource
                , value model.input.source
                , Css.View.input vc |> css
                , Locale.string vc.locale "Source" |> placeholder
                ]
                []
            ]
        , Dialog.part vc
            "Category"
            [ tc.entityConcepts
                |> List.sortBy .label
                |> List.map
                    (\c ->
                        option
                            [ value c.id
                            , c.id == model.input.category |> selected
                            ]
                            [ text c.label
                            ]
                    )
                |> (::) (option [ value "" ] [ text "" ])
                |> select
                    [ onInput UserInputsTagCategory
                    , Css.View.input vc |> css
                    ]
            ]
        , Dialog.part vc
            "Abuse"
            [ tc.abuseConcepts
                |> List.sortBy .label
                |> List.map
                    (\c ->
                        option
                            [ value c.id
                            , c.id == model.input.abuse |> selected
                            ]
                            [ text c.label
                            ]
                    )
                |> (::) (option [ value "" ] [ text "" ])
                |> select
                    [ onInput UserInputsTagAbuse
                    , Css.View.input vc |> css
                    ]
            ]
        , div
            [ Css.View.buttonsRow vc |> css
            ]
            [ input
                [ type_ "submit"
                , Css.Button.primary vc |> css
                , Locale.string vc.locale "Save" |> value
                ]
                []
            , model.existing
                |> Maybe.map
                    (\_ ->
                        input
                            [ type_ "button"
                            , Css.Button.danger vc |> css
                            , Locale.string vc.locale "Delete" |> value
                            , onClick UserClicksDeleteTag
                            ]
                            []
                    )
                |> Maybe.withDefault none
            ]
        ]
    ]
        |> div
            []
