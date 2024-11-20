module View.Search exposing (SearchConfig, SearchConfigWithMoreCss, default, search, searchWithMoreCss)

import Autocomplete
import Autocomplete.Styled as Autocomplete
import Config.View exposing (Config)
import Css exposing (Style)
import Css.Autocomplete
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
import String.Extra
import Util.View exposing (loadingSpinner)
import View.Autocomplete as Autocomplete
import View.Locale as Locale


type alias SearchConfig =
    { css : String -> List Style
    , resultsAsLink : Bool
    , multiline : Bool
    , showIcon : Bool
    }


type alias SearchConfigWithMoreCss =
    { css : String -> List Style
    , formCss : List Style
    , frameCss : List Style
    , button : List Style
    , resultLine : List Style
    , resultLineHighlighted : List Style
    , resultGroup : List Style
    , resultGroupTitle : List Style
    , resultLineIcon : List Style
    , resultTextEmphasized : List Style
    , resultsAsLink : Bool
    , dropdownFrame : List Style
    , dropdownResult : List Style
    , multiline : Bool
    , showIcon : Bool
    }


default : SearchConfigWithMoreCss
default =
    { css = \_ -> []
    , resultsAsLink = False
    , multiline = False
    , showIcon = False
    , formCss = []
    , frameCss = []
    , button = []
    , resultLine = []
    , resultLineHighlighted = []
    , resultGroup = []
    , resultGroupTitle = []
    , resultLineIcon = []
    , resultTextEmphasized = []
    , dropdownFrame = []
    , dropdownResult = []
    }


search : Plugins -> Config -> SearchConfig -> Model -> Html Msg
search plugins vc sc model =
    searchWithMoreCss plugins
        vc
        { css = sc.css
        , resultsAsLink = sc.resultsAsLink
        , multiline = sc.multiline
        , showIcon = sc.showIcon
        , formCss = []
        , frameCss = []
        , button = []
        , resultLine = []
        , resultLineHighlighted = []
        , resultGroup = []
        , resultGroupTitle = []
        , resultLineIcon = []
        , resultTextEmphasized = []
        , dropdownFrame = []
        , dropdownResult = []
        }
        model


searchWithMoreCss : Plugins -> Config -> SearchConfigWithMoreCss -> Model -> Html Msg
searchWithMoreCss plugins vc sc model =
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
        [ Css.form vc sc.showIcon |> css
        , css sc.formCss
        , stopPropagationOn "click" (Json.Decode.succeed ( NoOp, True ))
        , onSubmit UserClicksResultLine
        ]
        [ div
            [ Css.frame vc |> css
            , css sc.frameCss
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

                            SearchAddressAndTx _ ->
                                [ "Address", "transaction" ]
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
                , css sc.button
                , type_ "submit"
                ]
                [ FontAwesome.icon FontAwesome.search
                    |> Html.Styled.fromUnstyled
                ]

          else
            Util.View.none
        ]


searchResult : Plugins -> Config -> SearchConfigWithMoreCss -> Model -> Html Msg
searchResult plugins vc sc model =
    let
        viewState =
            Autocomplete.viewState model.autocomplete
    in
    if model.visible then
        resultList plugins vc sc model
            |> Autocomplete.dropdownStyled
                { frame = sc.dropdownFrame
                , result = sc.dropdownResult
                , loadingSpinner = loadingSpinner vc Css.Autocomplete.loadingSpinner
                }
                vc
                { loading = viewState.status == Autocomplete.Fetching
                , visible = model.visible
                , onClick = NoOp
                }

    else
        text ""


resultList : Plugins -> Config -> SearchConfigWithMoreCss -> Model -> List (Html Msg)
resultList _ vc sc { autocomplete, searchType } =
    let
        choices =
            Autocomplete.choices autocomplete
                |> List.indexedMap Tuple.pair

        q =
            (Autocomplete.viewState autocomplete).query

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
                    , css sc.resultGroup
                    ]
                    [ div
                        [ Css.resultGroupTitle vc |> css
                        , css sc.resultGroupTitle
                        ]
                        [ text title
                        ]
                    , List.map
                        (\( index, rl ) ->
                            resultLineToHtml vc
                                q
                                sc
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

        SearchAddressAndTx _ ->
            currencyBadges |> List.filterMap badgeToResult

        SearchAll _ ->
            currencyBadges
                ++ [ actorBadge
                   , labelBadge
                   ]
                |> List.filterMap badgeToResult



--++ Plugin.searchResultList plugins pluginStates vc


resultLineToHtml : Config -> String -> SearchConfigWithMoreCss -> Maybe ResultLine -> List (Attribute Msg) -> ResultLine -> Html Msg
resultLineToHtml vc query sc selectedValue choiceEvents resultLine =
    let
        ( icon, label ) =
            case resultLine of
                Address _ a ->
                    ( FontAwesome.at
                    , Util.View.truncate 50 a
                    )

                Tx _ a ->
                    ( FontAwesome.exchangeAlt
                    , Util.View.truncate 64 a
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
        ((Css.resultLine vc
            ++ (if selectedValue == Just resultLine then
                    Css.resultLineHighlighted vc
                        ++ sc.resultLineHighlighted

                else
                    []
               )
            |> css
         )
            :: css sc.resultLine
            :: choiceEvents
        )
        [ FontAwesome.icon icon
            |> Html.Styled.fromUnstyled
            |> List.singleton
            |> span
                [ Css.resultLineIcon vc |> css
                , css sc.resultLineIcon
                ]
        , if String.contains query label then
            span
                []
                [ text query
                , span
                    [ css
                        [ Css.fontWeight Css.bold
                        ]
                    , css sc.resultTextEmphasized
                    ]
                    [ text
                        (String.Extra.rightOf query label)
                    ]
                ]

          else
            text label
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
