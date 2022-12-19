module View.Graph.Search exposing (..)

import Config.View as View
import Css
import Css.Button
import Css.Graph as Css
import Css.View
import Html.Attributes as HA
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Input.Number
import Model.Graph.Search exposing (..)
import Msg.Graph exposing (Msg(..))
import Plugin.View exposing (Plugins)
import Tuple2 exposing (uncurry)
import View.Dialog as Dialog
import View.Locale as Locale


inputHovercard : Plugins -> View.Config -> Model -> Html Msg
inputHovercard plugins vc model =
    let
        current =
            criterionToString model.criterion

        dir =
            directionToString model.direction
    in
    [ Just UserClicksCloseSearchHovercard
        |> Dialog.headRow vc "Search neighbors"
    , Dialog.body vc
        { onSubmit = UserSubmitsSearchInput
        }
        [ Dialog.part vc
            "Direction"
            [ direction vc "outgoing" dir
            , direction vc "incoming" dir
            , direction vc "both" dir
            ]

        {- , Dialog.part vc
           "Criterion"
           [ criterion vc "category" current
           , criterion vc "addresses" current
           , criterion vc "final balance" current
           , criterion vc "total received" current
           ]
        -}
        , partByCriterion vc model.criterion
        , div
            [ Css.searchSettingsRow vc |> css
            ]
            [ Dialog.part vc
                "Depth"
                [ Input.Number.input
                    { onInput = UserInputsSearchDepth
                    , hasFocus = Nothing
                    , maxValue = Just 4
                    , minValue = Just 1
                    , maxLength = Just 1
                    }
                    (Css.View.inputRawWithLength vc 5
                        |> List.map (uncurry HA.style)
                    )
                    (Just model.depth)
                    |> Html.fromUnstyled
                ]
            , Dialog.part vc
                "Breadth"
                [ Input.Number.input
                    { onInput = UserInputsSearchBreadth
                    , hasFocus = Nothing
                    , maxValue = Just 100
                    , minValue = Just 1
                    , maxLength = Just 3
                    }
                    (Css.View.inputRawWithLength vc 6
                        |> List.map (uncurry HA.style)
                    )
                    (Just model.breadth)
                    |> Html.fromUnstyled
                ]
            , Dialog.part vc
                "Max. addresses"
                [ Input.Number.input
                    { onInput = UserInputsSearchMaxAddresses
                    , hasFocus = Nothing
                    , maxValue = Nothing
                    , minValue = Just 1
                    , maxLength = Nothing
                    }
                    (Css.View.inputRawWithLength vc 8
                        |> List.map (uncurry HA.style)
                    )
                    (Just model.maxAddresses)
                    |> Html.fromUnstyled
                ]
            ]
        , input
            [ type_ "submit"
            , Css.Button.primary vc |> css
            , Locale.string vc.locale "Search" |> value
            ]
            []
        ]
    ]
        |> div []


radio : View.Config -> { name : String, msg : String -> Msg, title : String, current : String } -> Html Msg
radio vc c =
    div
        [ Css.radio vc |> css
        ]
        [ input
            [ Css.radioInput vc |> css
            , type_ "radio"
            , name c.name
            , value c.title
            , onInput c.msg
            , c.current == c.title |> checked
            ]
            []
        , span
            [ Css.radioText vc |> css
            ]
            [ Locale.string vc.locale c.title
                |> text
            ]
        ]


direction : View.Config -> String -> String -> Html Msg
direction vc title current =
    radio vc
        { name = "direction"
        , title = title
        , current = current
        , msg = UserSelectsDirection
        }


criterion : View.Config -> String -> String -> Html Msg
criterion vc title current =
    radio vc
        { name = "criterion"
        , title = title
        , current = current
        , msg = UserSelectsCriterion
        }


partByCriterion : View.Config -> Criterion -> Html Msg
partByCriterion vc crit =
    case crit of
        Category categories active ->
            Dialog.part vc
                "Category"
                [ categories
                    |> List.sortBy .label
                    |> List.map
                        (\c ->
                            option
                                [ value c.id
                                , c.id == active |> selected
                                ]
                                [ text c.label
                                ]
                        )
                    |> select
                        [ onInput UserSelectsSearchCategory
                        , Css.View.input vc |> css
                        ]
                ]


criterionToString : Criterion -> String
criterionToString crit =
    case crit of
        Category _ _ ->
            "category"


directionToString : Direction -> String
directionToString dir =
    case dir of
        Incoming ->
            "incoming"

        Outgoing ->
            "outgoing"

        Both ->
            "both"
