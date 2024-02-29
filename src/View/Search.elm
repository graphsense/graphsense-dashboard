module View.Search exposing (search)

import Autocomplete
import Autocomplete.Styled as Autocomplete
import Config.View exposing (Config)
import Css exposing (Style)
import Css.Button
import Css.Search as Css
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import List.Extra
import Model.Search exposing (..)
import Msg.Search exposing (Msg(..))
import Plugin.View as Plugin exposing (Plugins)
import Util.View
import View.Autocomplete as Autocomplete
import View.Locale as Locale


type alias SearchConfig =
    { css : String -> List Style
    , resultsAsLink : Bool
    , multiline : Bool
    , showIcon : Bool
    }


search : Plugins -> Config -> SearchConfig -> Model -> Html Msg
search plugins vc sc model =
    let
        { inputEvents } =
            Autocomplete.events
                { onSelect = UserClicksResultLine
                , mapHtml = AutocompleteMsg
                }

        { query } =
            Autocomplete.viewState model.autocomplete
    in
    Html.Styled.form
        [ Css.form vc |> css
        , stopPropagationOn "click" (Json.Decode.succeed ( NoOp, True ))
        , onSubmit UserClicksResultLine
        ]
        [ div
            [ Css.frame vc |> css
            ]
            [ input
                ([ sc.css query |> css
                 , autocomplete False
                 , spellcheck False
                 , Locale.string vc.locale "The search" |> title
                 , onBlur UserLeavesSearch
                 , onFocus UserFocusSearch
                 , value query
                 ]
                    ++ inputEvents
                    ++ (case model.searchType of
                            SearchAll _ ->
                                [ "Address", "transaction", "label", "block", "actor" ]
                                    |> List.map (Locale.string vc.locale)
                                    |> (\st -> st ++ Plugin.searchPlaceholder plugins vc)
                                    |> String.join ", "
                                    |> placeholder
                                    |> List.singleton

                            SearchTagsOnly ->
                                [ Locale.string vc.locale "Label"
                                    |> placeholder
                                ]
                       )
                )
                []
            , searchResult plugins vc sc model
            ]
        , if sc.showIcon then
            button
                [ [ Css.Button.button vc |> Css.batch
                  , Css.Button.neutral vc |> Css.batch
                  , Css.button vc |> Css.batch
                  ]
                    |> css
                , type_ "submit"
                ]
                [ FontAwesome.icon FontAwesome.search
                    |> Html.Styled.fromUnstyled
                ]

          else
            Util.View.none
        ]


searchResult : Plugins -> Config -> SearchConfig -> Model -> Html Msg
searchResult plugins vc sc model =
    let
        viewState =
            Autocomplete.viewState model.autocomplete
    in
    if model.visible then
        resultList plugins vc sc model
            |> Autocomplete.dropdown vc
                { loading = viewState.status == Autocomplete.Fetching
                , visible = model.visible
                , onClick = NoOp
                }

    else
        text ""


resultList : Plugins -> Config -> SearchConfig -> Model -> List (Html Msg)
resultList plugins vc sc { autocomplete, searchType } =
    let
        choices =
            Autocomplete.choices autocomplete
                |> List.indexedMap Tuple.pair

        labelBadge =
            { title = Locale.string vc.locale "Labels"
            , badge =
                choices
                    |> List.filter
                        (\( _, rl ) ->
                            case rl of
                                Label _ ->
                                    True

                                _ ->
                                    False
                        )
            }

        actorBadge =
            { title = Locale.string vc.locale "Actors"
            , badge =
                choices
                    |> List.filter
                        (\( _, rl ) ->
                            case rl of
                                Actor _ ->
                                    True

                                _ ->
                                    False
                        )
            }

        currencyBadges =
            choices
                |> List.Extra.groupWhile
                    (\( _, a ) ( _, b ) -> resultLineCurrency a == resultLineCurrency b)
                |> List.filterMap
                    (\( fst, rest ) ->
                        Tuple.second fst
                            |> resultLineCurrency
                            |> Maybe.map
                                (\cur ->
                                    { title = String.toUpper cur
                                    , badge = fst :: rest
                                    }
                                )
                    )

        { choiceEvents } =
            Autocomplete.events
                { onSelect = UserClicksResultLine
                , mapHtml = AutocompleteMsg
                }

        selectedValue =
            Autocomplete.selectedValue autocomplete

        badgeToResult { title, badge } =
            if List.isEmpty badge then
                Nothing

            else
                div
                    [ Css.resultGroup vc |> css
                    ]
                    [ div
                        [ Css.resultGroupTitle vc |> css
                        ]
                        [ text title
                        ]
                    , List.map
                        (\( index, rl ) ->
                            resultLineToHtml vc
                                sc.resultsAsLink
                                selectedValue
                                (choiceEvents index)
                                rl
                        )
                        badge
                        |> ol [ Css.resultGroupList vc |> css ]
                    ]
                    |> Just
    in
    case searchType of
        SearchTagsOnly ->
            [ labelBadge ]
                |> List.filterMap badgeToResult

        SearchAll _ ->
            currencyBadges
                ++ [ actorBadge
                   , labelBadge
                   ]
                |> List.filterMap badgeToResult



--++ Plugin.searchResultList plugins pluginStates vc


resultLineToHtml : Config -> Bool -> Maybe ResultLine -> List (Attribute Msg) -> ResultLine -> Html Msg
resultLineToHtml vc asLink selectedValue choiceEvents resultLine =
    let
        ( icon, label ) =
            case resultLine of
                Address _ a ->
                    ( FontAwesome.at
                    , a
                    )

                Tx _ a ->
                    ( FontAwesome.exchangeAlt
                    , Util.View.truncate 50 a
                    )

                Block _ a ->
                    ( FontAwesome.cube
                    , String.fromInt a
                    )

                Label a ->
                    ( FontAwesome.tag, a )

                Actor ( _, lbl ) ->
                    ( FontAwesome.user, lbl )
    in
    span
        ([ Css.resultLine vc
            ++ (if selectedValue == Just resultLine then
                    Css.resultLineHighlighted vc

                else
                    []
               )
            |> css
         ]
            ++ choiceEvents
        )
        [ FontAwesome.icon icon
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> span [ Css.resultLineIcon vc |> css ]
        , text label
        ]


resultLineCurrency : ResultLine -> Maybe String
resultLineCurrency rl =
    case rl of
        Address currency _ ->
            Just currency

        Tx currency _ ->
            Just currency

        Block currency _ ->
            Just currency

        Label _ ->
            Nothing

        Actor _ ->
            Nothing
